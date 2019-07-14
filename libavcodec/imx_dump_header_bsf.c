/*
 * imx dump header bitstream filter
 * Copyright (c) 2007 Baptiste Coudurier
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

/**
 * @file
 * imx dump header bitstream filter
 * modifies bitstream to fit in mov and be decoded by final cut pro decoder
 */

#include "avcodec.h"
#include "bsf.h"
#include "bytestream.h"
#include "internal.h"


static int imx_dump_header(AVBSFContext *ctx, AVPacket *pkt)
{
    /* MXF essence element key */
    static const uint8_t imx_header[16] = { 0x06,0x0e,0x2b,0x34,0x01,0x02,0x01,0x01,0x0d,0x01,0x03,0x01,0x05,0x01,0x01,0x00 };

    AVBufferRef *out;
    int ret;
    uint8_t *out_buf;

    ret = ff_bsf_get_packet_ref(ctx, pkt);
    if (ret < 0)
        return ret;

    ret = ff_buffer_padded_alloc(&out, pkt->size, 20);
    if (ret < 0) {
        av_packet_unref(pkt);
        return ret;
    }

    out_buf = out->data;

    bytestream_put_buffer(&out_buf, imx_header, 16);
    bytestream_put_byte(&out_buf, 0x83); /* KLV BER long form */
    bytestream_put_be24(&out_buf, pkt->size);
    bytestream_put_buffer(&out_buf, pkt->data, pkt->size);

    ff_packet_replace_buffer(pkt, out);

    return 0;
}

static const enum AVCodecID codec_ids[] = {
    AV_CODEC_ID_MPEG2VIDEO, AV_CODEC_ID_NONE,
};

const AVBitStreamFilter ff_imx_dump_header_bsf = {
    .name      = "imxdump",
    .filter    = imx_dump_header,
    .codec_ids = codec_ids,
};
