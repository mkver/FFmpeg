/*
 * Musepack SV8 decoder
 * Copyright (c) 2007 Konstantin Shishkov
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
 * MPEG Audio Layer 1/2 -like codec with frames of 1152 samples
 * divided into 32 subbands.
 */

#include "libavutil/channel_layout.h"
#include "libavutil/lfg.h"
#include "avcodec.h"
#include "get_bits.h"
#include "internal.h"
#include "mpegaudiodsp.h"

#include "mpc.h"
#include "mpc8data.h"
#include "mpc8huff.h"

static VLC band_vlc, scfi_vlc[2], dscf_vlc[2], res_vlc[2];
static VLC q1_vlc, q2_vlc[2], q3_vlc[2], quant_vlc[4][2], q9up_vlc;

static inline int mpc8_dec_base(GetBitContext *gb, int k, int n)
{
    int len = mpc8_cnk_len[k-1][n-1] - 1;
    int code = len ? get_bits_long(gb, len) : 0;

    if (code >= mpc8_cnk_lost[k-1][n-1])
        code = ((code << 1) | get_bits1(gb)) - mpc8_cnk_lost[k-1][n-1];

    return code;
}

static inline int mpc8_dec_enum(GetBitContext *gb, int k, int n)
{
    int bits = 0;
    const uint32_t * C = mpc8_cnk[k-1];
    int code = mpc8_dec_base(gb, k, n);

    do {
        n--;
        if (code >= C[n]) {
            bits |= 1U << n;
            code -= C[n];
            C -= 32;
            k--;
        }
    } while(k > 0);

    return bits;
}

static inline int mpc8_get_mod_golomb(GetBitContext *gb, int m)
{
    if(mpc8_cnk_len[0][m] < 1) return 0;
    return mpc8_dec_base(gb, 1, m+1);
}

static int mpc8_get_mask(GetBitContext *gb, int size, int t)
{
    int mask = 0;

    if(t && t != size)
         mask = mpc8_dec_enum(gb, FFMIN(t, size - t), size);
    if((t << 1) > size) mask = ~mask;

    return mask;
}

static const uint16_t vlc_offsets[13] = {
    0, 640, 1184, 1748, 2298, 2426, 2490, 3002, 3258, 3786, 4298, 4876, 5388
};

static av_cold void build_vlc(VLC *vlc,
                              const uint8_t codes_counts[16],
                              const uint8_t syms[], int offset)
{
    uint8_t len[MPC8_MAX_VLC_SIZE];
    unsigned num = 0;

    for (int i = 16; i > 0; i--)
        for (unsigned tmp = num + codes_counts[i - 1]; num < tmp; num++)
            len[num] = i;

    ff_init_vlc_from_lengths(vlc, FFMIN(len[0], 9), num, len, 1,
                             syms, 1, 1, offset, INIT_VLC_USE_NEW_STATIC, NULL);
}

static av_cold int mpc8_decode_init(AVCodecContext * avctx)
{
    int i;
    MPCContext *c = avctx->priv_data;
    GetBitContext gb;
    static int vlc_initialized = 0;
    int channels;
    static VLC_TYPE codes_table[5388][2];

    if(avctx->extradata_size < 2){
        av_log(avctx, AV_LOG_ERROR, "Too small extradata size (%i)!\n", avctx->extradata_size);
        return -1;
    }
    memset(c->oldDSCF, 0, sizeof(c->oldDSCF));
    av_lfg_init(&c->rnd, 0xDEADBEEF);
    ff_mpadsp_init(&c->mpadsp);

    ff_mpc_init();

    init_get_bits(&gb, avctx->extradata, 16);

    skip_bits(&gb, 3);//sample rate
    c->maxbands = get_bits(&gb, 5) + 1;
    if (c->maxbands >= BANDS) {
        av_log(avctx,AV_LOG_ERROR, "maxbands %d too high\n", c->maxbands);
        return AVERROR_INVALIDDATA;
    }
    channels = get_bits(&gb, 4) + 1;
    if (channels > 2) {
        avpriv_request_sample(avctx, "Multichannel MPC SV8");
        return AVERROR_PATCHWELCOME;
    }
    c->MSS = get_bits1(&gb);
    c->frames = 1 << (get_bits(&gb, 3) * 2);

    avctx->sample_fmt = AV_SAMPLE_FMT_S16P;
    avctx->channel_layout = (channels==2) ? AV_CH_LAYOUT_STEREO : AV_CH_LAYOUT_MONO;
    avctx->channels = channels;

    if(vlc_initialized) return 0;
    av_log(avctx, AV_LOG_DEBUG, "Initing VLC\n");

#define INIT_VLC(vlc, bits, len_counts, symbols, offset, static_size) \
    do {                                                                   \
        static VLC_TYPE table[static_size][2];                             \
        (vlc)->table           = table;                                    \
        (vlc)->table_allocated = static_size;                              \
        build_vlc(vlc, len_counts, symbols, offset);                       \
    } while (0)


    INIT_VLC(&band_vlc, MPC8_BANDS_BITS,
             mpc8_bands_len_counts, mpc8_bands_syms, 0, 542);

    INIT_VLC(&q1_vlc, MPC8_Q1_BITS,
             mpc8_q1_len_counts, mpc8_q1_syms, 0, 520);
    INIT_VLC(&q9up_vlc, MPC8_Q9UP_BITS,
             mpc8_q9up_len_counts, mpc8_q9up_syms, 0, 524);

    INIT_VLC(&scfi_vlc[0], MPC8_SCFI0_BITS,
             mpc8_scfi_len_counts[0], mpc8_scfi0_syms, 0, 1 << MPC8_SCFI0_BITS);
    INIT_VLC(&scfi_vlc[1], MPC8_SCFI1_BITS,
             mpc8_scfi_len_counts[1], mpc8_scfi1_syms, 0, 1 << MPC8_SCFI1_BITS);

    INIT_VLC(&dscf_vlc[0], MPC8_DSCF0_BITS,
             mpc8_dscf_len_counts[0], mpc8_dscf0_syms, 0, 560);
    INIT_VLC(&dscf_vlc[1], MPC8_DSCF1_BITS,
             mpc8_dscf_len_counts[1], mpc8_dscf1_syms, 0, 598);

    INIT_VLC(&q3_vlc[0], MPC8_Q3_BITS,
             mpc8_q3_len_counts, mpc8_q3_syms, MPC8_Q3_OFFSET, 512);
    INIT_VLC(&q3_vlc[1], MPC8_Q4_BITS,
             mpc8_q4_len_counts, mpc8_q4_syms, MPC8_Q4_OFFSET, 516);

    for(i = 0; i < 2; i++){
        res_vlc[i].table = &codes_table[vlc_offsets[0+i]];
        res_vlc[i].table_allocated = vlc_offsets[1+i] - vlc_offsets[0+i];
        build_vlc(&res_vlc[i],
                  mpc8_res_len_counts[i], mpc8_res_syms[i], 0);

        q2_vlc[i].table = &codes_table[vlc_offsets[2+i]];
        q2_vlc[i].table_allocated = vlc_offsets[3+i] - vlc_offsets[2+i];
        build_vlc(&q2_vlc[i],
                  mpc8_q2_len_counts[i], mpc8_q2_syms[i], 0);

        quant_vlc[0][i].table = &codes_table[vlc_offsets[4+i]];
        quant_vlc[0][i].table_allocated = vlc_offsets[5+i] - vlc_offsets[4+i];
        build_vlc(&quant_vlc[0][i],
                  mpc8_q5_len_counts[i], mpc8_q5_syms[i], MPC8_Q5_OFFSET);
        quant_vlc[1][i].table = &codes_table[vlc_offsets[6+i]];
        quant_vlc[1][i].table_allocated = vlc_offsets[7+i] - vlc_offsets[6+i];
        build_vlc(&quant_vlc[1][i],
                  mpc8_q6_len_counts[i], mpc8_q6_syms[i], MPC8_Q6_OFFSET);
        quant_vlc[2][i].table = &codes_table[vlc_offsets[8+i]];
        quant_vlc[2][i].table_allocated = vlc_offsets[9+i] - vlc_offsets[8+i];
        build_vlc(&quant_vlc[2][i],
                  mpc8_q7_len_counts[i], mpc8_q7_syms[i], MPC8_Q7_OFFSET);
        quant_vlc[3][i].table = &codes_table[vlc_offsets[10+i]];
        quant_vlc[3][i].table_allocated = vlc_offsets[11+i] - vlc_offsets[10+i];
        build_vlc(&quant_vlc[3][i],
                  mpc8_q8_len_counts[i], mpc8_q8_syms[i], MPC8_Q8_OFFSET);
    }
    vlc_initialized = 1;

    return 0;
}

static int mpc8_decode_frame(AVCodecContext * avctx, void *data,
                             int *got_frame_ptr, AVPacket *avpkt)
{
    AVFrame *frame     = data;
    const uint8_t *buf = avpkt->data;
    int buf_size = avpkt->size;
    MPCContext *c = avctx->priv_data;
    GetBitContext gb2, *gb = &gb2;
    int i, j, k, ch, cnt, res, t;
    Band *bands = c->bands;
    int off;
    int maxband, keyframe;
    int last[2];

    keyframe = c->cur_frame == 0;

    if(keyframe){
        memset(c->Q, 0, sizeof(c->Q));
        c->last_bits_used = 0;
    }
    if ((res = init_get_bits8(gb, buf, buf_size)) < 0)
        return res;

    skip_bits(gb, c->last_bits_used & 7);

    if(keyframe)
        maxband = mpc8_get_mod_golomb(gb, c->maxbands + 1);
    else{
        maxband = c->last_max_band + get_vlc2(gb, band_vlc.table, MPC8_BANDS_BITS, 2);
        if(maxband > 32) maxband -= 33;
    }

    if (get_bits_left(gb) < 0) {
        *got_frame_ptr = 0;
        return buf_size;
    }

    if(maxband > c->maxbands + 1) {
        av_log(avctx, AV_LOG_ERROR, "maxband %d too large\n",maxband);
        return AVERROR_INVALIDDATA;
    }
    c->last_max_band = maxband;

    /* read subband indexes */
    if(maxband){
        last[0] = last[1] = 0;
        for(i = maxband - 1; i >= 0; i--){
            for(ch = 0; ch < 2; ch++){
                last[ch] = get_vlc2(gb, res_vlc[last[ch] > 2].table, MPC8_RES_BITS, 2) + last[ch];
                if(last[ch] > 15) last[ch] -= 17;
                bands[i].res[ch] = last[ch];
            }
        }
        if(c->MSS){
            int mask;

            cnt = 0;
            for(i = 0; i < maxband; i++)
                if(bands[i].res[0] || bands[i].res[1])
                    cnt++;
            t = mpc8_get_mod_golomb(gb, cnt);
            mask = mpc8_get_mask(gb, cnt, t);
            for(i = maxband - 1; i >= 0; i--)
                if(bands[i].res[0] || bands[i].res[1]){
                    bands[i].msf = mask & 1;
                    mask >>= 1;
                }
        }
    }
    for(i = maxband; i < c->maxbands; i++)
        bands[i].res[0] = bands[i].res[1] = 0;

    if(keyframe){
        for(i = 0; i < 32; i++)
            c->oldDSCF[0][i] = c->oldDSCF[1][i] = 1;
    }

    for(i = 0; i < maxband; i++){
        if(bands[i].res[0] || bands[i].res[1]){
            cnt = !!bands[i].res[0] + !!bands[i].res[1] - 1;
            if(cnt >= 0){
                t = get_vlc2(gb, scfi_vlc[cnt].table, scfi_vlc[cnt].bits, 1);
                if(bands[i].res[0]) bands[i].scfi[0] = t >> (2 * cnt);
                if(bands[i].res[1]) bands[i].scfi[1] = t & 3;
            }
        }
    }

    for(i = 0; i < maxband; i++){
        for(ch = 0; ch < 2; ch++){
            if(!bands[i].res[ch]) continue;

            if(c->oldDSCF[ch][i]){
                bands[i].scf_idx[ch][0] = get_bits(gb, 7) - 6;
                c->oldDSCF[ch][i] = 0;
            }else{
                t = get_vlc2(gb, dscf_vlc[1].table, MPC8_DSCF1_BITS, 2);
                if(t == 64)
                    t += get_bits(gb, 6);
                bands[i].scf_idx[ch][0] = ((bands[i].scf_idx[ch][2] + t - 25) & 0x7F) - 6;
            }
            for(j = 0; j < 2; j++){
                if((bands[i].scfi[ch] << j) & 2)
                    bands[i].scf_idx[ch][j + 1] = bands[i].scf_idx[ch][j];
                else{
                    t = get_vlc2(gb, dscf_vlc[0].table, MPC8_DSCF0_BITS, 2);
                    if(t == 31)
                        t = 64 + get_bits(gb, 6);
                    bands[i].scf_idx[ch][j + 1] = ((bands[i].scf_idx[ch][j] + t - 25) & 0x7F) - 6;
                }
            }
        }
    }

    for(i = 0, off = 0; i < maxband; i++, off += SAMPLES_PER_BAND){
        for(ch = 0; ch < 2; ch++){
            res = bands[i].res[ch];
            switch(res){
            case -1:
                for(j = 0; j < SAMPLES_PER_BAND; j++)
                    c->Q[ch][off + j] = (av_lfg_get(&c->rnd) & 0x3FC) - 510;
                break;
            case 0:
                break;
            case 1:
                for(j = 0; j < SAMPLES_PER_BAND; j += SAMPLES_PER_BAND / 2){
                    cnt = get_vlc2(gb, q1_vlc.table, MPC8_Q1_BITS, 2);
                    t = mpc8_get_mask(gb, 18, cnt);
                    for(k = 0; k < SAMPLES_PER_BAND / 2; k++)
                        c->Q[ch][off + j + k] = t & (1 << (SAMPLES_PER_BAND / 2 - k - 1))
                                                ? (get_bits1(gb) << 1) - 1 : 0;
                }
                break;
            case 2:
                cnt = 6;//2*mpc8_thres[res]
                for(j = 0; j < SAMPLES_PER_BAND; j += 3){
                    t = get_vlc2(gb, q2_vlc[cnt > 3].table, MPC8_Q2_BITS, 2);
                    c->Q[ch][off + j + 0] = mpc8_idx50[t];
                    c->Q[ch][off + j + 1] = mpc8_idx51[t];
                    c->Q[ch][off + j + 2] = mpc8_idx52[t];
                    cnt = (cnt >> 1) + mpc8_huffq2[t];
                }
                break;
            case 3:
            case 4:
                for(j = 0; j < SAMPLES_PER_BAND; j += 2){
                    t = get_vlc2(gb, q3_vlc[res - 3].table, MPC8_Q3_BITS, 2);
                    c->Q[ch][off + j + 1] = t >> 4;
                    c->Q[ch][off + j + 0] = sign_extend(t, 4);
                }
                break;
            case 5:
            case 6:
            case 7:
            case 8:
                cnt = 2 * mpc8_thres[res];
                for(j = 0; j < SAMPLES_PER_BAND; j++){
                    const VLC *vlc = &quant_vlc[res - 5][cnt > mpc8_thres[res]];
                    c->Q[ch][off + j] = get_vlc2(gb, vlc->table, vlc->bits, 2);
                    cnt = (cnt >> 1) + FFABS(c->Q[ch][off + j]);
                }
                break;
            default:
                for(j = 0; j < SAMPLES_PER_BAND; j++){
                    c->Q[ch][off + j] = get_vlc2(gb, q9up_vlc.table, MPC8_Q9UP_BITS, 2);
                    if(res != 9){
                        c->Q[ch][off + j] <<= res - 9;
                        c->Q[ch][off + j] |= get_bits(gb, res - 9);
                    }
                    c->Q[ch][off + j] -= (1 << (res - 2)) - 1;
                }
            }
        }
    }

    frame->nb_samples = MPC_FRAME_SIZE;
    if ((res = ff_get_buffer(avctx, frame, 0)) < 0)
        return res;

    ff_mpc_dequantize_and_synth(c, maxband - 1,
                                (int16_t **)frame->extended_data,
                                avctx->channels);

    c->cur_frame++;

    c->last_bits_used = get_bits_count(gb);
    if(c->cur_frame >= c->frames)
        c->cur_frame = 0;
    if (get_bits_left(gb) < 0) {
        av_log(avctx, AV_LOG_ERROR, "Overread %d\n", -get_bits_left(gb));
        c->last_bits_used = buf_size << 3;
    } else if (c->cur_frame == 0 && get_bits_left(gb) < 8) {// we have only padding left
        c->last_bits_used = buf_size << 3;
    }

    *got_frame_ptr = 1;

    return c->cur_frame ? c->last_bits_used >> 3 : buf_size;
}

static av_cold void mpc8_decode_flush(AVCodecContext *avctx)
{
    MPCContext *c = avctx->priv_data;
    c->cur_frame = 0;
}

AVCodec ff_mpc8_decoder = {
    .name           = "mpc8",
    .long_name      = NULL_IF_CONFIG_SMALL("Musepack SV8"),
    .type           = AVMEDIA_TYPE_AUDIO,
    .id             = AV_CODEC_ID_MUSEPACK8,
    .priv_data_size = sizeof(MPCContext),
    .init           = mpc8_decode_init,
    .decode         = mpc8_decode_frame,
    .flush          = mpc8_decode_flush,
    .capabilities   = AV_CODEC_CAP_DR1,
    .sample_fmts    = (const enum AVSampleFormat[]) { AV_SAMPLE_FMT_S16P,
                                                      AV_SAMPLE_FMT_NONE },
};
