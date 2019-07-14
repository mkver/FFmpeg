/*
 * MJPEG/AVI1 to JPEG/JFIF bitstream format filter
 * Copyright (c) 2010 Adrian Daerr and Nicolas George
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

/*
 * Adapted from mjpeg2jpeg.c, with original copyright:
 * Paris 2010 Adrian Daerr, public domain
 */

#include <string.h>

#include "libavutil/error.h"
#include "libavutil/mem.h"
#include "libavutil/intreadwrite.h"

#include "avcodec.h"
#include "bsf.h"
#include "jpegtables.h"
#include "mjpeg.h"

static const uint8_t jpeg_header[] = {
    0xff, 0xd8,                     // SOI
    0xff, 0xe0,                     // APP0
    0x00, 0x10,                     // APP0 header size (including
                                    // this field, but excluding preceding)
    0x4a, 0x46, 0x49, 0x46, 0x00,   // ID string 'JFIF\0'
    0x01, 0x01,                     // version
    0x00,                           // bits per type
    0x00, 0x00,                     // X density
    0x00, 0x00,                     // Y density
    0x00,                           // X thumbnail size
    0x00,                           // Y thumbnail size
};

static const int dht_segment_size = 420;
static const uint8_t dht_segment_head[] = { 0xFF, 0xC4, 0x01, 0xA2, 0x00 };
static const uint8_t dht_segment_frag[] = {
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
    0x0a, 0x0b, 0x01, 0x00, 0x03, 0x01, 0x01, 0x01, 0x01, 0x01,
    0x01, 0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
};

static uint8_t *append(uint8_t *buf, const uint8_t *src, int size)
{
    memcpy(buf, src, size);
    return buf + size;
}

static uint8_t *append_dht_segment(uint8_t *buf)
{
    buf = append(buf, dht_segment_head, sizeof(dht_segment_head));
    buf = append(buf, avpriv_mjpeg_bits_dc_luminance + 1, 16);
    buf = append(buf, dht_segment_frag, sizeof(dht_segment_frag));
    buf = append(buf, avpriv_mjpeg_val_dc, 12);
    *(buf++) = 0x10;
    buf = append(buf, avpriv_mjpeg_bits_ac_luminance + 1, 16);
    buf = append(buf, avpriv_mjpeg_val_ac_luminance, 162);
    *(buf++) = 0x11;
    buf = append(buf, avpriv_mjpeg_bits_ac_chrominance + 1, 16);
    buf = append(buf, avpriv_mjpeg_val_ac_chrominance, 162);
    return buf;
}

static int mjpeg2jpeg_filter(AVBSFContext *ctx, AVPacket *pkt)
{
    AVBufferRef *out = NULL;
    int ret;
    int input_skip, size_diff;
    uint8_t *output;

    ret = ff_bsf_get_packet_ref(ctx, pkt);
    if (ret < 0)
        return ret;

    if (pkt->size < 12) {
        av_log(ctx, AV_LOG_ERROR, "input is truncated\n");
        ret = AVERROR_INVALIDDATA;
        goto fail;
    }
    if (AV_RB16(pkt->data) != 0xffd8) {
        av_log(ctx, AV_LOG_ERROR, "input is not MJPEG\n");
        ret = AVERROR_INVALIDDATA;
        goto fail;
    }
    if (pkt->data[2] == 0xff && pkt->data[3] == APP0) {
        input_skip = (pkt->data[4] << 8) + pkt->data[5] + 4;
    } else {
        input_skip = 2;
    }
    if (pkt->size < input_skip) {
        av_log(ctx, AV_LOG_ERROR, "input is truncated\n");
        ret = AVERROR_INVALIDDATA;
        goto fail;
    }
    size_diff = sizeof(jpeg_header) + dht_segment_size - input_skip;
    ret = ff_buffer_padded_realloc(&out, pkt->size, size_diff);
    if (ret < 0)
        goto fail;

    output = out->data;

    output = append(output, jpeg_header, sizeof(jpeg_header));
    output = append_dht_segment(output);
    output = append(output, pkt->data + input_skip, pkt->size - input_skip);

    ff_packet_replace_buffer(pkt, out);

fail:
    if (ret < 0)
        av_packet_unref(pkt);
    return ret;
}

static const enum AVCodecID codec_ids[] = {
    AV_CODEC_ID_MJPEG, AV_CODEC_ID_NONE,
};

const AVBitStreamFilter ff_mjpeg2jpeg_bsf = {
    .name           = "mjpeg2jpeg",
    .filter         = mjpeg2jpeg_filter,
    .codec_ids      = codec_ids,
};
