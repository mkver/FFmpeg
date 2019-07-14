/*
 * Vp9 invisible (alt-ref) frame to superframe merge bitstream filter
 * Copyright (c) 2016 Ronald S. Bultje <rsbultje@gmail.com>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "libavutil/avassert.h"
#include "avcodec.h"
#include "bsf.h"
#include "get_bits.h"
#include "internal.h"

#define MAX_CACHE 8
typedef struct VP9BSFContext {
    AVBufferRef *cache[MAX_CACHE];
    int n_cache;
} VP9BSFContext;

static void stats(AVBufferRef * const *in, int n_in,
                  unsigned *_max, unsigned *_sum)
{
    int n;
    unsigned max = 0, sum = 0;

    for (n = 0; n < n_in; n++) {
        unsigned sz = in[n]->size;

        if (sz > max)
            max = sz;
        sum += sz;
    }

    *_max = max;
    *_sum = sum;
}

static int merge_superframe(AVBufferRef * const *in, int n_in, AVPacket *pkt)
{
    unsigned max, sum, mag, marker, n;
    AVBufferRef *out;
    uint8_t *ptr;
    int res;

    stats(in, n_in, &max, &sum);
    mag = av_log2(max) >> 3;
    marker = 0xC0 + (mag << 3) + (n_in - 1);
    res = ff_buffer_padded_alloc(&out, sum, 2 + (mag + 1) * n_in);
    if (res < 0)
        return res;
    ptr = out->data;
    for (n = 0; n < n_in; n++) {
        memcpy(ptr, in[n]->data, in[n]->size);
        ptr += in[n]->size;
    }

#define wloop(mag, wr) \
    do { \
        for (n = 0; n < n_in; n++) { \
            wr; \
            ptr += mag + 1; \
        } \
    } while (0)

    // write superframe with marker 110[mag:2][nframes:3]
    *ptr++ = marker;
    switch (mag) {
    case 0:
        wloop(mag, *ptr = in[n]->size);
        break;
    case 1:
        wloop(mag, AV_WL16(ptr, in[n]->size));
        break;
    case 2:
        wloop(mag, AV_WL24(ptr, in[n]->size));
        break;
    case 3:
        wloop(mag, AV_WL32(ptr, in[n]->size));
        break;
    }
    *ptr++ = marker;
    av_assert0(ptr == &out->data[out->size]);

    ff_packet_replace_buffer(pkt, out);

    return 0;
}

static int vp9_superframe_filter(AVBSFContext *ctx, AVPacket *pkt)
{
    GetBitContext gb;
    VP9BSFContext *s = ctx->priv_data;
    int res, invisible, profile, marker, uses_superframe_syntax = 0, n;

    res = ff_bsf_get_packet_ref(ctx, pkt);
    if (res < 0)
        return res;

    marker = pkt->data[pkt->size - 1];
    if ((marker & 0xe0) == 0xc0) {
        int nbytes = 1 + ((marker >> 3) & 0x3);
        int n_frames = 1 + (marker & 0x7), idx_sz = 2 + n_frames * nbytes;

        uses_superframe_syntax = pkt->size >= idx_sz && pkt->data[pkt->size - idx_sz] == marker;
    }

    if ((res = init_get_bits8(&gb, pkt->data, pkt->size)) < 0)
        goto done;

    get_bits(&gb, 2); // frame marker
    profile  = get_bits1(&gb);
    profile |= get_bits1(&gb) << 1;
    if (profile == 3) profile += get_bits1(&gb);

    if (get_bits1(&gb)) {
        invisible = 0;
    } else {
        get_bits1(&gb); // keyframe
        invisible = !get_bits1(&gb);
    }

    if (uses_superframe_syntax && s->n_cache > 0) {
        av_log(ctx, AV_LOG_ERROR,
               "Mixing of superframe syntax and naked VP9 frames not supported\n");
        res = AVERROR(ENOSYS);
        goto done;
    } else if ((!invisible || uses_superframe_syntax) && !s->n_cache) {
        // passthrough
        return 0;
    } else if (s->n_cache + 1 >= MAX_CACHE) {
        av_log(ctx, AV_LOG_ERROR,
               "Too many invisible frames\n");
        res = AVERROR_INVALIDDATA;
        goto done;
    }

    // move the packet's buffer into the cache
    pkt->buf->data = pkt->data;
    pkt->buf->size = pkt->size;
    s->cache[s->n_cache++] = pkt->buf;
    pkt->buf = NULL;

    if (invisible) {
        res = AVERROR(EAGAIN);
        goto done;
    }
    av_assert0(s->n_cache > 0);

    // build superframe
    if ((res = merge_superframe(s->cache, s->n_cache, pkt)) < 0)
        goto done;

    for (n = 0; n < s->n_cache; n++)
        av_buffer_unref(&s->cache[n]);
    s->n_cache = 0;

done:
    if (res < 0)
        av_packet_unref(pkt);
    return res;
}

static void vp9_superframe_close_flush(AVBSFContext *ctx)
{
    VP9BSFContext *s = ctx->priv_data;
    int n;

    // unref cached data
    for (n = 0; n < s->n_cache; n++)
        av_buffer_unref(&s->cache[n]);
    s->n_cache = 0;
}

static const enum AVCodecID codec_ids[] = {
    AV_CODEC_ID_VP9, AV_CODEC_ID_NONE,
};

const AVBitStreamFilter ff_vp9_superframe_bsf = {
    .name           = "vp9_superframe",
    .priv_data_size = sizeof(VP9BSFContext),
    .filter         = vp9_superframe_filter,
    .flush          = vp9_superframe_close_flush,
    .close          = vp9_superframe_close_flush,
    .codec_ids      = codec_ids,
};
