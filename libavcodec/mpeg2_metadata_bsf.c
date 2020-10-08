/*
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

#include "libavutil/avstring.h"
#include "libavutil/common.h"
#include "libavutil/opt.h"
#define UNCHECKED_BITSTREAM_READER 1
#include "bsf.h"
#include "cbs.h"
#include "cbs_bsf.h"
#include "cbs_mpeg2.h"
#include "get_bits.h"
#include "mpeg12.h"
#include "mpeg12vlc.h"

#define MB_QUANT (1 << 0)
#define MB_FOR   (1 << 1)
#define MB_BACK  (1 << 2)
#define MB_PAT   (1 << 3)
#define MB_INTRA (1 << 4)

enum {
    PROGRESSIVE_FRAMES   =   1 << 0,
    SWAP_TBFF            =   1 << 1,
    NO_REPEAT            =   1 << 2,
    PROGRESSIVE_SEQUENCE =   1 << 3,
    TRIM_PADDING         =   1 << 4,
};

typedef struct MPEG2MetadataContext {
    CBSBSFContext common;

    MPEG2RawExtensionData sequence_display_extension;

    AVRational display_aspect_ratio;

    AVRational frame_rate;

    int flags;
    unsigned progressivable_frames;

    int video_format;
    int colour_primaries;
    int transfer_characteristics;
    int matrix_coefficients;

    int mpeg1_warned;
} MPEG2MetadataContext;

typedef struct SliceContext {
    uint8_t mb_type;
    uint8_t motion_vector_count;
    uint8_t frame_mv;
    uint8_t dmv;

    uint8_t picture_coding_type;

    uint8_t f_code[2][2];
    uint8_t picture_structure;
    uint8_t frame_pred_frame_dct;
    uint8_t concealment_motion_vectors;
    uint8_t intra_vlc_format;
    uint8_t progressivable;

    uint8_t block_count; /* derived from chroma_format */
} SliceContext;

static const uint8_t ptype2mb_type[7] = {
                                           MB_INTRA,
                                  MB_PAT,
               MB_FOR,
               MB_FOR |           MB_PAT,
    MB_QUANT |                             MB_INTRA,
    MB_QUANT |                    MB_PAT,
    MB_QUANT | MB_FOR |           MB_PAT,
};

static const uint8_t btype2mb_type[11] = {
                                           MB_INTRA,
                        MB_BACK,
                        MB_BACK | MB_PAT,
               MB_FOR,
               MB_FOR |           MB_PAT,
               MB_FOR | MB_BACK,
               MB_FOR | MB_BACK | MB_PAT,
    MB_QUANT |                             MB_INTRA,
    MB_QUANT |          MB_BACK | MB_PAT,
    MB_QUANT | MB_FOR |           MB_PAT,
    MB_QUANT | MB_FOR | MB_BACK | MB_PAT
};

static int macroblock_modes(SliceContext *ctx, GetBitContext *gb)
{
    int mb_type;

    switch (ctx->picture_coding_type) {
    case 1: /* I */
        if (get_bits1(gb) == 0) {
            if (get_bits1(gb) == 0)
                return AVERROR_INVALIDDATA;
            mb_type = MB_QUANT | MB_INTRA;
        } else {
            mb_type = MB_INTRA;
        }
        break;
    case 2: /* P */
        mb_type = get_vlc2(gb, ff_mb_ptype_vlc.table, MB_PTYPE_VLC_BITS, 1);
        if (mb_type < 0)
            return AVERROR_INVALIDDATA;
        mb_type = ptype2mb_type[mb_type];
        break;
    case 3: /* B */
        mb_type = get_vlc2(gb, ff_mb_btype_vlc.table, MB_BTYPE_VLC_BITS, 1);
        if (mb_type < 0)
            return AVERROR_INVALIDDATA;
        mb_type = btype2mb_type[mb_type];
        break;
    }
    ctx->mb_type = mb_type;

    /* spatial_temporal_weight_code omitted */

    if (ctx->mb_type & (MB_FOR|MB_BACK)) {
        int motion_type;
        if (ctx->picture_structure == PICT_FRAME) {
            if (!ctx->frame_pred_frame_dct)
                motion_type = get_bits(gb, 2);
            else
                motion_type = 2;
            ctx->motion_vector_count = 1 + (motion_type == 1);
            ctx->frame_mv = motion_type == 2;
            if (motion_type != 2)
                ctx->progressivable = 0;
        } else {
            motion_type = get_bits(gb, 2);
            ctx->motion_vector_count = 1 + (motion_type == 2);
            ctx->frame_mv = 0;
        }
        ctx->dmv = motion_type == 3;
    } else {
        /* Might be used for MB_INTRA with concealment_motion_vectors */
        ctx->dmv = 0;
        ctx->motion_vector_count = 1;
        ctx->frame_mv = ctx->picture_structure == PICT_FRAME;
    }

    if ((ctx->picture_structure == PICT_FRAME) &&
        !ctx->frame_pred_frame_dct &&
        ctx->mb_type & (MB_INTRA|MB_PAT)) {
        if (get_bits1(gb)) /* dct_type */
            ctx->progressivable = 0;
    }

    return 0;
}

static int parse_motion_vector(SliceContext *ctx, GetBitContext *gb, int backwards)
{
    int motion_code;

    motion_code = get_vlc2(gb, ff_mv_vlc.table, MV_VLC_BITS, 2);
    if (motion_code < 0)
        return AVERROR_INVALIDDATA;
    if (motion_code) {
        /* The motion_code read is actually missing the last bit
         * which contains the sign of the value read. We ignore this
         * and instead skip f_code[backwards][0] bits here instead of
         * f_code[backwards][0] - 1 as per the spec. */
        skip_bits(gb, ctx->f_code[backwards][0]);
    }
    if (ctx->dmv) {
        if (get_bits1(gb))
            skip_bits1(gb);
    }
    motion_code = get_vlc2(gb, ff_mv_vlc.table, MV_VLC_BITS, 2);
    if (motion_code < 0)
        return AVERROR_INVALIDDATA;
    if (motion_code) {
        skip_bits(gb, ctx->f_code[backwards][1]);
    }
    if (ctx->dmv) {
        if (get_bits1(gb))
            skip_bits1(gb);
    }
    return 0;
}

static int parse_motion_vectors(SliceContext *ctx, GetBitContext *gb, int backwards)
{
    int ret;

    if (ctx->motion_vector_count == 1) {
        if (!ctx->frame_mv && !ctx->dmv)
            skip_bits1(gb); /* motion_vertical_field_select[0][backwards] */
        ret = parse_motion_vector(ctx, gb, backwards);
        if (ret < 0)
            return ret;
    } else {
        skip_bits1(gb); /* motion_vertical_field_select[0][backwards] */
        ret = parse_motion_vector(ctx, gb, backwards);
        if (ret < 0)
            return ret;
        skip_bits1(gb); /* motion_vertical_field_select[1][backwards] */
        ret = parse_motion_vector(ctx, gb, backwards);
        if (ret < 0)
            return ret;
    }
    return 0;
}

static int parse_dc_coeffs(SliceContext *ctx, GetBitContext *gb, const RLTable *rl)
{
    av_unused int level, run;
    OPEN_READER(re, gb);

    while (1) {
        UPDATE_CACHE(re, gb);
        GET_RL_VLC(level, run, re, gb, rl->rl_vlc[0],
                   TEX_VLC_BITS, 2, 0);
        if (level == 127) {
            break;
        } else if (!level) {
            LAST_SKIP_BITS(re, gb, 18); /* Skip escaped */
            continue;
        } else if (level == MAX_LEVEL)
            return AVERROR_INVALIDDATA;
        LAST_SKIP_BITS(re, gb, 1);
    };
    CLOSE_READER(re, gb);

    return 0;
}

static int parse_block(SliceContext *ctx, GetBitContext *gb, int cbp, int i)
{
    const RLTable *rl = &ff_rl_mpeg1;

    if (ctx->mb_type & MB_INTRA) {
        int skip = get_vlc2(gb, i < 4 ? ff_dc_lum_vlc.table
                                      : ff_dc_chroma_vlc.table,
                            DC_VLC_BITS, 2);
        skip_bits(gb, skip);
        if (ctx->intra_vlc_format)
            rl = &ff_rl_mpeg2;
    } else if (cbp & (1 << i)) {
        if (show_bits1(gb) == 1)
            skip_bits(gb, 2);
    } else
        return 0;

    return parse_dc_coeffs(ctx, gb, rl);
}

static int parse_blocks(SliceContext *ctx, GetBitContext *gb)
{
    int cbp, ret;

    if (ctx->mb_type & MB_PAT) {
        cbp = get_vlc2(gb, ff_mb_pat_vlc.table, MB_PAT_VLC_BITS, 1);
        if (cbp < 0)
            return AVERROR_INVALIDDATA;
        if (ctx->block_count > 6)
            cbp |= get_bits(gb, ctx->block_count - 6) << 6;
        /* Hint: The order of the bits will be messed up and won't
         * correspond directly to pattern_code[i], but it doesn't matter
         * as for MB_INTRA all bits are 1 whereas for non-intra
         * only the number of set bits matters. */
    } else if (!(ctx->mb_type & MB_INTRA))
        return 0;

    for (int i = 0; i < ctx->block_count; i++) {
        ret = parse_block(ctx, gb, cbp, i);
        if (ret < 0)
            return ret;
    }
    return 0;
}

static int parse_macroblock(SliceContext *ctx, GetBitContext *gb)
{
    int incr, ret;

    do {
        incr = get_vlc2(gb, ff_mbincr_vlc.table, MBINCR_VLC_BITS, 2);
        if (incr > 33U)
            return AVERROR_INVALIDDATA;
    } while (incr == 33);

    ret = macroblock_modes(ctx, gb);
    if (ret < 0)
        return ret;

    if (ctx->mb_type & MB_QUANT)
        skip_bits(gb, 5); /* quantiser_scale_code */

    if (ctx->mb_type & MB_FOR ||
        (ctx->mb_type & MB_INTRA && ctx->concealment_motion_vectors)) {
        ret = parse_motion_vectors(ctx, gb, 0);
        if (ret < 0)
            return ret;
    }
    if (ctx->mb_type & MB_BACK) {
        ret = parse_motion_vectors(ctx, gb, 1);
        if (ret < 0)
            return ret;
    }

    if (ctx->mb_type & MB_INTRA && ctx->concealment_motion_vectors)
        skip_bits1(gb); /* marker_bit (set to 1) */

    ret = parse_blocks(ctx, gb);
    if (ret < 0)
        return ret;

    return 0;
}

static int parse_slice_data(SliceContext *ctx, MPEG2RawSlice *slice, void *log)
{
    GetBitContext gb;
    int ret;

    init_get_bits8(&gb, slice->data, slice->data_size);
    skip_bits(&gb, slice->data_bit_start);

    do {
        ret = parse_macroblock(ctx, &gb);
        if (ret < 0)
            return ret;
    } while (get_bits_left(&gb) > 0 && show_bits(&gb, FFMIN(23, get_bits_left(&gb))) != 0);

    if (get_bits_left(&gb) < 0) {
        av_log(log, AV_LOG_ERROR, "Truncated slice\n");
        return AVERROR_INVALIDDATA;
    }

    slice->data_size = (get_bits_count(&gb) + 7) >> 3;

    return 0;
}

static void update_slices(AVBSFContext *bsf, CodedBitstreamFragment *frag)
{
    MPEG2MetadataContext *const ctx = bsf->priv_data;
    const CodedBitstreamMPEG2Context *const mpeg2 = ctx->common.input->priv_data;
    const MPEG2RawPictureHeader *pic = NULL;
    const MPEG2RawPictureCodingExtension *pic_ext = NULL;
    SliceContext slice;
    int ret;

    if (!mpeg2->chroma_format)
        return;

    slice.block_count = 4 + (1 << mpeg2->chroma_format);
    for (int i = 0; i < frag->nb_units; i++) {
        CodedBitstreamUnit *unit = &frag->units[i];

        if (MPEG2_START_IS_SLICE(unit->type)) {
            if (!pic || !pic_ext)
                return;
            ret = parse_slice_data(&slice, unit->content, bsf);
            if (ret < 0) {
                av_log(bsf, AV_LOG_ERROR, "Error parsing slice %"PRIX32"\n",
                       unit->type);
                return;
            }
        } else if (unit->type == MPEG2_START_PICTURE) {
            if (pic)
                return;
            pic = unit->content;
            if (pic->picture_coding_type > 3)
                return;
            slice.picture_coding_type = pic->picture_coding_type;
        } else if (unit->type == MPEG2_START_EXTENSION) {
            const MPEG2RawExtensionData *ext = unit->content;
            if (ext->extension_start_code_identifier !=
                    MPEG2_EXTENSION_PICTURE_CODING)
                continue;
            if (pic_ext)
                return;
            pic_ext = &ext->data.picture_coding;

            for (int s = 0; s < 2; s++)
                for (int t = 0; t < 2; t++)
                    if (pic_ext->f_code[s][t] >= 10 &&
                        pic_ext->f_code[s][t] <= 14)
                        return;
            memcpy(slice.f_code, pic_ext->f_code, sizeof(slice.f_code));

            if (!pic_ext->picture_structure)
                return;
            slice.picture_structure          = pic_ext->picture_structure;

            slice.frame_pred_frame_dct       = pic_ext->frame_pred_frame_dct;
            slice.concealment_motion_vectors = pic_ext->concealment_motion_vectors;
            slice.intra_vlc_format           = pic_ext->intra_vlc_format;
            slice.progressivable = slice.picture_structure == PICT_FRAME && !slice.frame_pred_frame_dct;
        }
    }
    if (!pic_ext)
        return;
    if (slice.progressivable)
        ctx->progressivable_frames++;
}

static int mpeg2_metadata_update_fragment(AVBSFContext *bsf,
                                          AVPacket *pkt,
                                          CodedBitstreamFragment *frag)
{
    MPEG2MetadataContext             *ctx = bsf->priv_data;
    MPEG2RawSequenceHeader            *sh = NULL;
    MPEG2RawSequenceExtension         *se = NULL;
    MPEG2RawSequenceDisplayExtension *sde = NULL;
    int i, se_pos;

    if (ctx->flags & TRIM_PADDING)
        update_slices(bsf, frag);

    for (i = 0; i < frag->nb_units; i++) {
        if (frag->units[i].type == MPEG2_START_SEQUENCE_HEADER) {
            sh = frag->units[i].content;
        } else if (frag->units[i].type == MPEG2_START_EXTENSION) {
            MPEG2RawExtensionData *ext = frag->units[i].content;
            if (ext->extension_start_code_identifier ==
                MPEG2_EXTENSION_SEQUENCE) {
                se = &ext->data.sequence;
                se_pos = i;
            } else if (ext->extension_start_code_identifier ==
                MPEG2_EXTENSION_SEQUENCE_DISPLAY) {
                sde = &ext->data.sequence_display;
            }
        }
    }

    if (!sh || !se) {
        // No sequence header and sequence extension: not an MPEG-2 video
        // sequence.
        if (sh && !ctx->mpeg1_warned) {
            av_log(bsf, AV_LOG_WARNING, "Stream contains a sequence "
                   "header but not a sequence extension: maybe it's "
                   "actually MPEG-1?\n");
            ctx->mpeg1_warned = 1;
        }
        goto no_sequence_header;
    }

    if (ctx->flags & PROGRESSIVE_SEQUENCE) {
        if (!se->progressive_sequence) {
            if ((sh->vertical_size_value + 15) / 16 & 1U) {
                av_log(bsf, AV_LOG_ERROR, "16 or more pixels of vertical "
                       "padding are incompatible with progressive_sequence.\n");
                return AVERROR(EINVAL);
            }
            se->progressive_sequence = 1;
        }
    }

    if (ctx->display_aspect_ratio.num && ctx->display_aspect_ratio.den) {
        int num, den;

        av_reduce(&num, &den, ctx->display_aspect_ratio.num,
                  ctx->display_aspect_ratio.den, 65535);

        if (num == 4 && den == 3)
            sh->aspect_ratio_information = 2;
        else if (num == 16 && den == 9)
            sh->aspect_ratio_information = 3;
        else if (num == 221 && den == 100)
            sh->aspect_ratio_information = 4;
        else
            sh->aspect_ratio_information = 1;
    }

    if (ctx->frame_rate.num && ctx->frame_rate.den) {
        int code, ext_n, ext_d;

        ff_mpeg12_find_best_frame_rate(ctx->frame_rate,
                                       &code, &ext_n, &ext_d, 0);

        sh->frame_rate_code        = code;
        se->frame_rate_extension_n = ext_n;
        se->frame_rate_extension_d = ext_d;
    }

    if (ctx->video_format             >= 0 ||
        ctx->colour_primaries         >= 0 ||
        ctx->transfer_characteristics >= 0 ||
        ctx->matrix_coefficients      >= 0) {
        if (!sde) {
            int err;
            ctx->sequence_display_extension.extension_start_code =
                MPEG2_START_EXTENSION;
            ctx->sequence_display_extension.extension_start_code_identifier =
                MPEG2_EXTENSION_SEQUENCE_DISPLAY;
            sde = &ctx->sequence_display_extension.data.sequence_display;

            *sde = (MPEG2RawSequenceDisplayExtension) {
                .video_format = 5,

                .colour_description       = 0,
                .colour_primaries         = 2,
                .transfer_characteristics = 2,
                .matrix_coefficients      = 2,

                .display_horizontal_size =
                    se->horizontal_size_extension << 12 | sh->horizontal_size_value,
                .display_vertical_size =
                    se->vertical_size_extension << 12 | sh->vertical_size_value,
            };

            err = ff_cbs_insert_unit_content(frag, se_pos + 1,
                                             MPEG2_START_EXTENSION,
                                             &ctx->sequence_display_extension,
                                             NULL);
            if (err < 0) {
                av_log(bsf, AV_LOG_ERROR, "Failed to insert new sequence "
                       "display extension.\n");
                return err;
            }
        }

        if (ctx->video_format >= 0)
            sde->video_format = ctx->video_format;

        if (ctx->colour_primaries         >= 0 ||
            ctx->transfer_characteristics >= 0 ||
            ctx->matrix_coefficients      >= 0) {
            sde->colour_description = 1;

            if (ctx->colour_primaries >= 0)
                sde->colour_primaries = ctx->colour_primaries;

            if (ctx->transfer_characteristics >= 0)
                sde->transfer_characteristics = ctx->transfer_characteristics;

            if (ctx->matrix_coefficients >= 0)
                sde->matrix_coefficients = ctx->matrix_coefficients;
        }
    }

no_sequence_header:
    if (ctx->flags & (PROGRESSIVE_FRAMES|SWAP_TBFF|NO_REPEAT)) {
        const CodedBitstreamMPEG2Context *mpeg2 = ctx->common.input->priv_data;
        int change_progressive = !mpeg2->progressive_sequence && ctx->flags & PROGRESSIVE_SEQUENCE;
        int progressive        =  mpeg2->progressive_sequence || ctx->flags & PROGRESSIVE_SEQUENCE;

        for (int i = 0; i < frag->nb_units; i++) {
            MPEG2RawExtensionData *ext;
            MPEG2RawPictureCodingExtension *pic;

            if (frag->units[i].type != MPEG2_START_EXTENSION)
                continue;

            ext = frag->units[i].content;
            if (ext->extension_start_code_identifier != MPEG2_EXTENSION_PICTURE_CODING)
                continue;

            pic = &ext->data.picture_coding;

            if (ctx->flags & PROGRESSIVE_SEQUENCE &&
                pic->picture_structure != PICT_FRAME) {
                av_log(bsf, AV_LOG_ERROR, "Field picture "
                       "incompatible with progressive_sequence.\n");
                return AVERROR(EINVAL);
            }

            if (ctx->flags & PROGRESSIVE_SEQUENCE &&
                !pic->frame_pred_frame_dct) {
                av_log(bsf, AV_LOG_ERROR, "Frame with frame_pred_frame_dct"
                       " unset incompatible with progressive_sequence.\n");
                return AVERROR(EINVAL);
            }

            if (change_progressive) {
                /* Reset these flags as their semantics changes
                 * to repeating the frames. */
                pic->top_field_first    = 0;
                pic->repeat_first_field = 0;
            }

            if (pic->picture_structure == PICT_FRAME) {
                if (ctx->flags & NO_REPEAT)
                    pic->repeat_first_field = 0;
                if (ctx->flags & NO_REPEAT && progressive)
                    pic->top_field_first = 0;
                else if (ctx->flags & SWAP_TBFF && !progressive)
                    pic->top_field_first = !pic->top_field_first;

                if (ctx->flags & PROGRESSIVE_FRAMES)
                    pic->progressive_frame = 1;

                if (mpeg2->chroma_format == 1)
                    pic->chroma_420_type = pic->progressive_frame;
            }
            break;
        }
    }

    return 0;
}

static const CBSBSFType mpeg2_metadata_type = {
    .codec_id        = AV_CODEC_ID_MPEG2VIDEO,
    .fragment_name   = "frame",
    .unit_name       = "start code",
    .update_fragment = &mpeg2_metadata_update_fragment,
};

static int mpeg2_metadata_init(AVBSFContext *bsf)
{
    MPEG2MetadataContext *ctx = bsf->priv_data;
    int field_order;

#define VALIDITY_CHECK(name) do { \
        if (!ctx->name) { \
            av_log(bsf, AV_LOG_ERROR, "The value 0 for %s is " \
                                      "forbidden.\n", #name); \
            return AVERROR(EINVAL); \
        } \
    } while (0)
    VALIDITY_CHECK(colour_primaries);
    VALIDITY_CHECK(transfer_characteristics);
    VALIDITY_CHECK(matrix_coefficients);
#undef VALIDITY_CHECK

    if (ctx->flags & TRIM_PADDING)
        ff_mpeg12_init_vlcs();

    field_order = (ctx->flags & PROGRESSIVE_SEQUENCE)  ? AV_FIELD_PROGRESSIVE
                : (bsf->par_out->field_order != AV_FIELD_PROGRESSIVE
                   && ctx->flags & PROGRESSIVE_FRAMES) ? AV_FIELD_UNKNOWN : -1;

    ff_cbs_update_video_parameters(ctx->common.output, bsf->par_out, -1, -1, -1,
                                   -1, field_order, -1, ctx->colour_primaries,
                                   ctx->transfer_characteristics,
                                   ctx->matrix_coefficients, -1, -1);

    return ff_cbs_bsf_generic_init(bsf, &mpeg2_metadata_type);
}

static void mpeg2_metadata_close(AVBSFContext *bsf)
{
    MPEG2MetadataContext *ctx = bsf->priv_data;

    if (ctx->flags & TRIM_PADDING)
        av_log(bsf, AV_LOG_INFO, "%u frames are lacking frame_pred_frame_dct.\n",
               ctx->progressivable_frames);

    ff_cbs_bsf_generic_close(bsf);
}

#define OFFSET(x) offsetof(MPEG2MetadataContext, x)
#define FLAGS (AV_OPT_FLAG_VIDEO_PARAM|AV_OPT_FLAG_BSF_PARAM)
static const AVOption mpeg2_metadata_options[] = {
    { "display_aspect_ratio", "Set display aspect ratio (table 6-3)",
        OFFSET(display_aspect_ratio), AV_OPT_TYPE_RATIONAL,
        { .dbl = 0.0 }, 0, 65535, FLAGS },

    { "frame_rate", "Set frame rate",
        OFFSET(frame_rate), AV_OPT_TYPE_RATIONAL,
        { .dbl = 0.0 }, 0, UINT_MAX, FLAGS },

    { "flags", "flags",
        OFFSET(flags), AV_OPT_TYPE_FLAGS,
        { .i64 = 0 }, 0, INT_MAX, FLAGS, "flags" },
    { "progressive_frames", "Mark all coded frames as progressive",
        0, AV_OPT_TYPE_CONST, { .i64 = PROGRESSIVE_FRAMES },
        0, INT_MAX, FLAGS, "flags" },
    { "swap_t/bff", "Swap TFF/BFF in coded frames",
        0, AV_OPT_TYPE_CONST, { .i64 = SWAP_TBFF },
        0, INT_MAX, FLAGS, "flags" },
    { "no_repeat", "Set flags to not repeat fields/frames",
        0, AV_OPT_TYPE_CONST, { .i64 = NO_REPEAT },
        0, INT_MAX, FLAGS, "flags" },
    { "progressive_sequence", "Mark sequence and all frames as progressive",
        0, AV_OPT_TYPE_CONST, { .i64 = PROGRESSIVE_FRAMES|PROGRESSIVE_SEQUENCE },
        0, INT_MAX, FLAGS, "flags" },
    { "trim_padding", "Trim padding bits (slow)",
        0, AV_OPT_TYPE_CONST, { .i64 = TRIM_PADDING },
        0, INT_MAX, FLAGS, "flags" },

    { "video_format", "Set video format (table 6-6)",
        OFFSET(video_format), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, 7, FLAGS },
    { "colour_primaries", "Set colour primaries (table 6-7)",
        OFFSET(colour_primaries), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, 255, FLAGS },
    { "transfer_characteristics", "Set transfer characteristics (table 6-8)",
        OFFSET(transfer_characteristics), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, 255, FLAGS },
    { "matrix_coefficients", "Set matrix coefficients (table 6-9)",
        OFFSET(matrix_coefficients), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, 255, FLAGS },

    { NULL }
};

static const AVClass mpeg2_metadata_class = {
    .class_name = "mpeg2_metadata_bsf",
    .item_name  = av_default_item_name,
    .option     = mpeg2_metadata_options,
    .version    = LIBAVUTIL_VERSION_INT,
};

static const enum AVCodecID mpeg2_metadata_codec_ids[] = {
    AV_CODEC_ID_MPEG2VIDEO, AV_CODEC_ID_NONE,
};

const AVBitStreamFilter ff_mpeg2_metadata_bsf = {
    .name           = "mpeg2_metadata",
    .priv_data_size = sizeof(MPEG2MetadataContext),
    .priv_class     = &mpeg2_metadata_class,
    .init           = &mpeg2_metadata_init,
    .close          = &mpeg2_metadata_close,
    .filter         = &ff_cbs_bsf_generic_filter,
    .codec_ids      = mpeg2_metadata_codec_ids,
};
