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

#include "bsf.h"
#include "cbs.h"
#include "cbs_bsf.h"
#include "cbs_mpeg2.h"
#include "mpeg12.h"

enum {
    PROGRESSIVE_FRAMES   =   1 << 0,
    SWAP_TBFF            =   1 << 1,
    NO_REPEAT            =   1 << 2,
    PROGRESSIVE_SEQUENCE =   1 << 3,
};

typedef struct MPEG2MetadataContext {
    CBSBSFContext common;

    MPEG2RawExtensionData sequence_display_extension;

    AVRational display_aspect_ratio;

    AVRational frame_rate;

    int flags;

    int video_format;
    int colour_primaries;
    int transfer_characteristics;
    int matrix_coefficients;

    int mpeg1_warned;
} MPEG2MetadataContext;


static int mpeg2_metadata_update_fragment(AVBSFContext *bsf,
                                          AVPacket *pkt,
                                          CodedBitstreamFragment *frag)
{
    MPEG2MetadataContext             *ctx = bsf->priv_data;
    MPEG2RawSequenceHeader            *sh = NULL;
    MPEG2RawSequenceExtension         *se = NULL;
    MPEG2RawSequenceDisplayExtension *sde = NULL;
    int i, se_pos;

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

    field_order = (ctx->flags & PROGRESSIVE_SEQUENCE)  ? AV_FIELD_PROGRESSIVE
                : (bsf->par_out->field_order != AV_FIELD_PROGRESSIVE
                   && ctx->flags & PROGRESSIVE_FRAMES) ? AV_FIELD_UNKNOWN : -1;

    ff_cbs_update_video_parameters(ctx->common.output, bsf->par_out, -1, -1, -1,
                                   -1, field_order, -1, ctx->colour_primaries,
                                   ctx->transfer_characteristics,
                                   ctx->matrix_coefficients, -1, -1);

    return ff_cbs_bsf_generic_init(bsf, &mpeg2_metadata_type);
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
    .close          = &ff_cbs_bsf_generic_close,
    .filter         = &ff_cbs_bsf_generic_filter,
    .codec_ids      = mpeg2_metadata_codec_ids,
};
