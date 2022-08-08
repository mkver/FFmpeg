/*
 * Mpeg video formats-related picture management functions
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

#include <stdint.h>

#include "libavutil/avassert.h"
#include "libavutil/common.h"
#include "libavutil/pixdesc.h"
#include "libavutil/imgutils.h"

#include "avcodec.h"
#include "encode.h"
#include "decode.h"
#include "motion_est.h"
#include "mpegpicture.h"
#include "mpegutils.h"
#include "refstruct.h"
#include "threadframe.h"

static void av_noinline free_picture_tables(Picture *pic)
{
    pic->alloc_mb_width  =
    pic->alloc_mb_height = 0;

    ff_refstruct_unref(&pic->mbskip_table);
    ff_refstruct_unref(&pic->qscale_table_base);
    ff_refstruct_unref(&pic->mb_type_base);

    for (int i = 0; i < 2; i++) {
        ff_refstruct_unref(&pic->motion_val_base[i]);
        ff_refstruct_unref(&pic->ref_index[i]);
    }
}

int ff_mpeg_framesize_alloc(AVCodecContext *avctx, MotionEstContext *me,
                            ScratchpadContext *sc, int linesize)
{
#   define EMU_EDGE_HEIGHT (4 * 70)
    int alloc_size = FFALIGN(FFABS(linesize) + 64, 32);

    if (avctx->hwaccel)
        return 0;

    if (linesize < 24) {
        av_log(avctx, AV_LOG_ERROR, "Image too small, temporary buffers cannot function\n");
        return AVERROR_PATCHWELCOME;
    }

    if (av_image_check_size2(alloc_size, EMU_EDGE_HEIGHT, avctx->max_pixels, AV_PIX_FMT_NONE, 0, avctx) < 0)
        return AVERROR(ENOMEM);

    // edge emu needs blocksize + filter length - 1
    // (= 17x17 for  halfpel / 21x21 for H.264)
    // VC-1 computes luma and chroma simultaneously and needs 19X19 + 9x9
    // at uvlinesize. It supports only YUV420 so 24x24 is enough
    // linesize * interlaced * MBsize
    // we also use this buffer for encoding in encode_mb_internal() needig an additional 32 lines
    if (!FF_ALLOCZ_TYPED_ARRAY(sc->edge_emu_buffer, alloc_size * EMU_EDGE_HEIGHT) ||
        !FF_ALLOCZ_TYPED_ARRAY(me->scratchpad,      alloc_size * 4 * 16 * 2)) {
        av_freep(&sc->edge_emu_buffer);
        return AVERROR(ENOMEM);
    }

    me->temp            = me->scratchpad;
    sc->rd_scratchpad   = me->scratchpad;
    sc->b_scratchpad    = me->scratchpad;
    sc->obmc_scratchpad = me->scratchpad + 16;

    return 0;
}

/**
 * Allocate a frame buffer
 */
static int alloc_frame_buffer(AVCodecContext *avctx,  Picture *pic,
                              MotionEstContext *me, ScratchpadContext *sc,
                              int chroma_x_shift, int chroma_y_shift,
                              int linesize, int uvlinesize)
{
    int edges_needed = av_codec_is_encoder(avctx->codec);
    int r, ret;

    pic->tf.f = pic->f;

    if (edges_needed) {
        pic->f->width  = avctx->width  + 2 * EDGE_WIDTH;
        pic->f->height = avctx->height + 2 * EDGE_WIDTH;

        r = ff_encode_alloc_frame(avctx, pic->f);
    } else if (avctx->codec_id != AV_CODEC_ID_WMV3IMAGE &&
               avctx->codec_id != AV_CODEC_ID_VC1IMAGE  &&
               avctx->codec_id != AV_CODEC_ID_MSS2) {
        r = ff_thread_get_ext_buffer(avctx, &pic->tf,
                                     pic->reference ? AV_GET_BUFFER_FLAG_REF : 0);
    } else {
        pic->f->width  = avctx->width;
        pic->f->height = avctx->height;
        pic->f->format = avctx->pix_fmt;
        r = avcodec_default_get_buffer2(avctx, pic->f, 0);
    }

    if (r < 0 || !pic->f->buf[0]) {
        av_log(avctx, AV_LOG_ERROR, "get_buffer() failed (%d %p)\n",
               r, pic->f->data[0]);
        return -1;
    }

    if (edges_needed) {
        int i;
        for (i = 0; pic->f->data[i]; i++) {
            int offset = (EDGE_WIDTH >> (i ? chroma_y_shift : 0)) *
                         pic->f->linesize[i] +
                         (EDGE_WIDTH >> (i ? chroma_x_shift : 0));
            pic->f->data[i] += offset;
        }
        pic->f->width  = avctx->width;
        pic->f->height = avctx->height;
    }

    ret = ff_hwaccel_frame_priv_alloc(avctx, &pic->hwaccel_picture_private);
    if (ret < 0)
        return ret;

    if ((linesize   &&   linesize != pic->f->linesize[0]) ||
        (uvlinesize && uvlinesize != pic->f->linesize[1])) {
        av_log(avctx, AV_LOG_ERROR,
               "get_buffer() failed (stride changed: linesize=%d/%d uvlinesize=%d/%d)\n",
               linesize,   pic->f->linesize[0],
               uvlinesize, pic->f->linesize[1]);
        ff_mpeg_unref_picture(avctx, pic);
        return -1;
    }

    if (av_pix_fmt_count_planes(pic->f->format) > 2 &&
        pic->f->linesize[1] != pic->f->linesize[2]) {
        av_log(avctx, AV_LOG_ERROR,
               "get_buffer() failed (uv stride mismatch)\n");
        ff_mpeg_unref_picture(avctx, pic);
        return -1;
    }

    if (!sc->edge_emu_buffer &&
        (ret = ff_mpeg_framesize_alloc(avctx, me, sc,
                                       pic->f->linesize[0])) < 0) {
        av_log(avctx, AV_LOG_ERROR,
               "get_buffer() failed to allocate context scratch buffers.\n");
        ff_mpeg_unref_picture(avctx, pic);
        return ret;
    }

    return 0;
}

static int alloc_picture_tables(BufferPoolContext *pools, Picture *pic,
                                int mb_stride, int mb_width, int mb_height)
{
#define GET_BUFFER(name, buf_suffix, idx_suffix) do { \
    pic->name ## buf_suffix idx_suffix = ff_refstruct_pool_get(pools->name ## _pool); \
    if (!pic->name ## buf_suffix idx_suffix) \
        return AVERROR(ENOMEM); \
} while (0)
    GET_BUFFER(mbskip_table,,);
    GET_BUFFER(qscale_table, _base,);
    GET_BUFFER(mb_type, _base,);
    if (pools->motion_val_pool) {
        for (int i = 0; i < 2; i++) {
            GET_BUFFER(motion_val, _base, [i]);
            GET_BUFFER(ref_index,, [i]);
        }
    }
#undef GET_BUFFER

    pic->alloc_mb_width  = mb_width;
    pic->alloc_mb_height = mb_height;
    pic->alloc_mb_stride = mb_stride;

    return 0;
}

/**
 * Allocate a Picture.
 * The pixels are allocated/set by calling get_buffer() if shared = 0
 */
int ff_alloc_picture(AVCodecContext *avctx, Picture *pic, MotionEstContext *me,
                     ScratchpadContext *sc, int shared, int encoding,
                     int chroma_x_shift, int chroma_y_shift, int out_format,
                     BufferPoolContext *pools,
                     int mb_stride, int mb_width, int mb_height, int b8_stride,
                     ptrdiff_t *linesize, ptrdiff_t *uvlinesize)
{
    int i, ret;
    av_assert0(!pic->mbskip_table);
    if (shared) {
        av_assert0(pic->f->data[0]);
        pic->shared = 1;
    } else {
        av_assert0(!pic->f->buf[0]);
        if (alloc_frame_buffer(avctx, pic, me, sc,
                               chroma_x_shift, chroma_y_shift,
                               *linesize, *uvlinesize) < 0)
            return -1;

        *linesize   = pic->f->linesize[0];
        *uvlinesize = pic->f->linesize[1];
    }

    ret = alloc_picture_tables(pools, pic,
                               mb_stride, mb_width, mb_height);
    if (ret < 0)
        goto fail;

    pic->qscale_table = pic->qscale_table_base + 2 * mb_stride + 1;
    pic->mb_type      = pic->mb_type_base      + 2 * mb_stride + 1;

    if (pic->motion_val_base[0]) {
        for (i = 0; i < 2; i++) {
            pic->motion_val[i] = pic->motion_val_base[i] + 4;
        }
    }

    return 0;
fail:
    av_log(avctx, AV_LOG_ERROR, "Error allocating a picture.\n");
    ff_mpeg_unref_picture(avctx, pic);
    return AVERROR(ENOMEM);
}

/**
 * Deallocate a picture; frees the picture tables in case they
 * need to be reallocated anyway.
 */
void ff_mpeg_unref_picture(AVCodecContext *avctx, Picture *pic)
{
    pic->tf.f = pic->f;
    /* WM Image / Screen codecs allocate internal buffers with different
     * dimensions / colorspaces; ignore user-defined callbacks for these. */
    if (avctx->codec_id != AV_CODEC_ID_WMV3IMAGE &&
        avctx->codec_id != AV_CODEC_ID_VC1IMAGE  &&
        avctx->codec_id != AV_CODEC_ID_MSS2)
        ff_thread_release_ext_buffer(avctx, &pic->tf);
    else if (pic->f)
        av_frame_unref(pic->f);

    ff_refstruct_unref(&pic->hwaccel_picture_private);

    free_picture_tables(pic);

    pic->field_picture = 0;
    pic->b_frame_score = 0;
    pic->reference     = 0;
    pic->shared        = 0;
    pic->display_picture_number = 0;
    pic->coded_picture_number   = 0;
}

static void update_picture_tables(Picture *dst, const Picture *src)
{
    ff_refstruct_replace(&dst->mbskip_table, src->mbskip_table);
    ff_refstruct_replace(&dst->qscale_table_base, src->qscale_table_base);
    ff_refstruct_replace(&dst->mb_type_base,      src->mb_type_base);
    for (int i = 0; i < 2; i++) {
        ff_refstruct_replace(&dst->motion_val_base[i], src->motion_val_base[i]);
        ff_refstruct_replace(&dst->ref_index[i],  src->ref_index[i]);
    }

    dst->qscale_table  = src->qscale_table;
    dst->mb_type       = src->mb_type;
    for (int i = 0; i < 2; i++)
        dst->motion_val[i] = src->motion_val[i];

    dst->alloc_mb_width  = src->alloc_mb_width;
    dst->alloc_mb_height = src->alloc_mb_height;
    dst->alloc_mb_stride = src->alloc_mb_stride;
}

int ff_mpeg_ref_picture(AVCodecContext *avctx, Picture *dst, Picture *src)
{
    int ret;

    av_assert0(!dst->f->buf[0]);
    av_assert0(src->f->buf[0]);

    src->tf.f = src->f;
    dst->tf.f = dst->f;
    ret = ff_thread_ref_frame(&dst->tf, &src->tf);
    if (ret < 0)
        goto fail;

    update_picture_tables(dst, src);

    ff_refstruct_replace(&dst->hwaccel_picture_private,
                          src->hwaccel_picture_private);

    dst->field_picture           = src->field_picture;
    dst->b_frame_score           = src->b_frame_score;
    dst->reference               = src->reference;
    dst->shared                  = src->shared;
    dst->display_picture_number  = src->display_picture_number;
    dst->coded_picture_number    = src->coded_picture_number;

    return 0;
fail:
    ff_mpeg_unref_picture(avctx, dst);
    return ret;
}

Picture *ff_get_unused_picture(void *logctx, Picture picture[])
{
    for (int i = 0; i < MAX_PICTURE_COUNT; i++) {
        if (!picture[i].f->buf[0])
            return &picture[i];
    }

    av_log(logctx, AV_LOG_FATAL,
           "Internal error, picture buffer overflow\n");
    /* We could return -1, but the codec would crash trying to draw into a
     * non-existing frame anyway. This is safer than waiting for a random crash.
     * Also the return of this is never useful, an encoder must only allocate
     * as much as allowed in the specification. This has no relationship to how
     * much libavcodec could allocate (and MAX_PICTURE_COUNT is always large
     * enough for such valid streams).
     * Plus, a decoder has to check stream validity and remove frames if too
     * many reference frames are around. Waiting for "OOM" is not correct at
     * all. Similarly, missing reference frames have to be replaced by
     * interpolated/MC frames, anything else is a bug in the codec ...
     */
    abort();
    return NULL;
}

void av_cold ff_mpv_picture_free(AVCodecContext *avctx, Picture *pic)
{
    ff_mpeg_unref_picture(avctx, pic);
    av_frame_free(&pic->f);
}
