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
#include "libavutil/display.h"
#include "libavutil/common.h"
#include "libavutil/opt.h"

#include "bsf.h"
#include "bsf_internal.h"
#include "cbs.h"
#include "cbs_h264.h"
#include "h264.h"
#include "h264_levels.h"
#include "h264_sei.h"

enum {
    MINIMIZE,
    MINIMIZE_2,
    INSERT_2,
    INSERT_3,
    UNDETERMINED,
    PASS,
    INSERT,
    REMOVE,
    EXTRACT,
};

enum {
    DEFAULT_PRESET,
    PASSTHROUGH_PRESET,
    ARD_PRESET,
    ZDF_PRESET,
    ITUNES_PRESET,
};

enum {
    MIN_IDR                    =   1,
    MIN_SLICE_TYPE             =   2,
    SINGLE_QP                  =   4,
    MOD_REF                    =   8,
    MOD_FRAME_NUM              =  16,
    MOD_POC                    =  32,
    PROGRESSIVE                =  64,
};

enum {
    IS_IN_EXTRA                =   1,
    IS_IN_HEADER               =   2,
    DIFFERS_OUT                =   4,
    DIFFERS_HEADER             =   8,
    UNWRITTEN_AFTER_SPS_CHANGE =  16,
    WRITTEN_THIS_GOP           =  32,
    WRITE                      =  64,
    HEADER_WAS_USED            = 128,
    DIFFERS_OUT_EFF            = DIFFERS_OUT|UNWRITTEN_AFTER_SPS_CHANGE,
    HEADER_FIRST_USE           = IS_IN_HEADER|DIFFERS_HEADER|HEADER_WAS_USED,
};

enum {
    FLIP_HORIZONTAL = 1,
    FLIP_VERTICAL   = 2,
};

enum {
    LEVEL_UNSET = -2,
    LEVEL_AUTO  = -1,
};

typedef struct PSPack {
    void        *ps;
    AVBufferRef *ps_ref;
} PSPack;

typedef struct Extradata {
    PSPack sps[H264_MAX_SPS_COUNT];
    PSPack pps[H264_MAX_PPS_COUNT];
    uint8_t sps_status[H264_MAX_SPS_COUNT];
    uint8_t pps_status[H264_MAX_PPS_COUNT];
    int max_sps;
    int max_pps;
} Extradata;

typedef struct Header {
    PSPack sps[H264_MAX_SPS_COUNT];
    PSPack pps[H264_MAX_PPS_COUNT];
    int max_sps;
    int max_pps;
} Header;

typedef struct H264MetadataContext {
    const AVClass *class;

    CodedBitstreamContext *input;
    CodedBitstreamContext *output;
    CodedBitstreamFragment access_unit;

    int done_first_au;

    int aud;
    H264RawAUD aud_nal;

    AVRational sample_aspect_ratio;

    int overscan_appropriate_flag;

    int video_format;
    int video_full_range_flag;
    int colour_primaries;
    int transfer_characteristics;
    int matrix_coefficients;

    int chroma_sample_loc_type;

    AVRational tick_rate;
    int fixed_frame_rate_flag;
    int zero_new_constraint_set_flags;

    int crop_left;
    int crop_right;
    int crop_top;
    int crop_bottom;

    int reorder_frames;

    const char *sei_user_data;
    SEIRawUserDataUnregistered sei_user_data_payload;

    int frame_num_offset;
    int poc_offset;

    int delete_filler;

    int display_orientation;
    double rotate;
    int flip;
    H264RawSEIDisplayOrientation display_orientation_payload;

    union {
        int init;
        struct {
            uint8_t state;
            uint8_t ref[2];
        } ref;
    } ref;
    union {
        int init;
        struct {
            uint8_t flags;
            uint8_t idr;
            uint8_t interlaced;
        } misc;
    } misc;

    union {
        int init;
        struct {
            int8_t state; // > 0: several qp's, == 0: qp unused, < 0: single qp
            int8_t num_qp_minus_1;
            int8_t weighted_pred_flag;
            int8_t qp_val[5];
        } qp;
    } qp;
    int *qp_histogram;
    int8_t *qp_lut;

    int level;

    int extradata;
    Extradata *extra;
    Header *header;
    int extra_status;
    int header_unused;

    int dts;
    int64_t last_dts;

    int preset;
} H264MetadataContext;

static int compare_ps(const void *ps_1, const void *ps_2, int is_sps)
{
    size_t size = is_sps ? sizeof(H264RawSPS) : sizeof(H264RawPPS);

    if (!ps_2)
        return 1;

    return memcmp(ps_1, ps_2, size);
}

static int h264_metadata_insert_aud(AVBSFContext *bsf,
                                    CodedBitstreamFragment *au)
{
    H264MetadataContext *ctx = bsf->priv_data;
    int primary_pic_type_mask = 0xff;
    int err, i, j;

    static const int primary_pic_type_table[] = {
        0x084, // 2, 7
        0x0a5, // 0, 2, 5, 7
        0x0e7, // 0, 1, 2, 5, 6, 7
        0x210, // 4, 9
        0x318, // 3, 4, 8, 9
        0x294, // 2, 4, 7, 9
        0x3bd, // 0, 2, 3, 4, 5, 7, 8, 9
        0x3ff, // 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
    };

    for (i = 0; i < au->nb_units; i++) {
        if (au->units[i].type == H264_NAL_SLICE ||
            au->units[i].type == H264_NAL_IDR_SLICE) {
            H264RawSlice *slice = au->units[i].content;
            for (j = 0; j < FF_ARRAY_ELEMS(primary_pic_type_table); j++) {
                if (!(primary_pic_type_table[j] &
                      (1 << slice->header.slice_type)))
                    primary_pic_type_mask &= ~(1 << j);
            }
        }
    }
    for (j = 0; j < FF_ARRAY_ELEMS(primary_pic_type_table); j++)
        if (primary_pic_type_mask & (1 << j))
            break;
    if (j >= FF_ARRAY_ELEMS(primary_pic_type_table)) {
        av_log(bsf, AV_LOG_ERROR, "No usable primary_pic_type: "
               "invalid slice types?\n");
        return AVERROR_INVALIDDATA;
    }

    ctx->aud_nal = (H264RawAUD) {
        .nal_unit_header.nal_unit_type = H264_NAL_AUD,
        .primary_pic_type = j,
    };

    err = ff_cbs_insert_unit_content(au, 0, H264_NAL_AUD,
                                     &ctx->aud_nal, NULL);
    if (err < 0) {
        av_log(bsf, AV_LOG_ERROR, "Failed to insert AUD.\n");
        return err;
    }

    return 0;
}

static int h264_metadata_update_extradata(AVBSFContext *bsf, AVPacket *out)
{
    H264MetadataContext   *ctx = bsf->priv_data;
    CodedBitstreamFragment *au = &ctx->access_unit;
    Extradata *extra = ctx->extra;
    Header   *header = ctx->header;
    uint8_t *data;
    int err;

    for (int i = 0; i <= header->max_sps; i++) {
        if ((extra->sps_status[i] & (IS_IN_HEADER|HEADER_WAS_USED)) == IS_IN_HEADER) {
            av_log(bsf, AV_LOG_VERBOSE, "%cPS with id %d was unused.\n", 'S', i);
        } else {
            err = ff_cbs_insert_unit_content(au, -1, H264_NAL_SPS,
                                             header->sps[i].ps,
                                             header->sps[i].ps_ref);
            if (err < 0)
                return err;
        }
    }
    for (int i = 0; i <= header->max_pps; i++) {
        if ((extra->pps_status[i] & (IS_IN_HEADER|HEADER_WAS_USED)) == IS_IN_HEADER) {
            av_log(bsf, AV_LOG_VERBOSE, "%cPS with id %d was unused.\n", 'P', i);
        } else {
            err = ff_cbs_insert_unit_content(au, -1, H264_NAL_PPS,
                                             header->pps[i].ps,
                                             header->pps[i].ps_ref);
            if (err < 0)
                return err;
        }
    }

    err = ff_cbs_write_fragment_data(ctx->output, au);
    if (err < 0)
        return err;

    data = av_packet_new_side_data(out, AV_PKT_DATA_NEW_EXTRADATA, au->data_size);
    if (!data)
        return AVERROR(ENOMEM);

    memcpy(data, au->data, au->data_size);
    if (ctx->last_dts != AV_NOPTS_VALUE)
        out->dts = out->pts = ctx->last_dts + 1;

    return 0;
}

static int h264_metadata_update_sps(AVBSFContext *bsf,
                                    CodedBitstreamUnit *unit)
{
    H264MetadataContext *ctx = bsf->priv_data;
    H264RawSPS *sps = unit->content;
    int need_vui = 0;
    int crop_unit_x, crop_unit_y;

    if (ctx->sample_aspect_ratio.num && ctx->sample_aspect_ratio.den) {
        // Table E-1.
        static const AVRational sar_idc[] = {
            {   0,  0 }, // Unspecified (never written here).
            {   1,  1 }, {  12, 11 }, {  10, 11 }, {  16, 11 },
            {  40, 33 }, {  24, 11 }, {  20, 11 }, {  32, 11 },
            {  80, 33 }, {  18, 11 }, {  15, 11 }, {  64, 33 },
            { 160, 99 }, {   4,  3 }, {   3,  2 }, {   2,  1 },
        };
        int num, den, i;

        av_reduce(&num, &den, ctx->sample_aspect_ratio.num,
                  ctx->sample_aspect_ratio.den, 65535);

        for (i = 1; i < FF_ARRAY_ELEMS(sar_idc); i++) {
            if (num == sar_idc[i].num &&
                den == sar_idc[i].den)
                break;
        }
        if (i == FF_ARRAY_ELEMS(sar_idc)) {
            sps->vui.aspect_ratio_idc = 255;
            sps->vui.sar_width  = num;
            sps->vui.sar_height = den;
        } else {
            sps->vui.aspect_ratio_idc = i;
        }
        sps->vui.aspect_ratio_info_present_flag = 1;
        need_vui = 1;
    }

#define SET_VUI_FIELD(field) do { \
        if (ctx->field >= 0) { \
            sps->vui.field = ctx->field; \
            need_vui = 1; \
        } \
    } while (0)

    if (ctx->overscan_appropriate_flag >= 0) {
        SET_VUI_FIELD(overscan_appropriate_flag);
        sps->vui.overscan_info_present_flag = 1;
    }

    if (ctx->video_format             >= 0 ||
        ctx->video_full_range_flag    >= 0 ||
        ctx->colour_primaries         >= 0 ||
        ctx->transfer_characteristics >= 0 ||
        ctx->matrix_coefficients      >= 0) {

        SET_VUI_FIELD(video_format);

        SET_VUI_FIELD(video_full_range_flag);

        if (ctx->colour_primaries         >= 0 ||
            ctx->transfer_characteristics >= 0 ||
            ctx->matrix_coefficients      >= 0) {

            SET_VUI_FIELD(colour_primaries);
            SET_VUI_FIELD(transfer_characteristics);
            SET_VUI_FIELD(matrix_coefficients);

            sps->vui.colour_description_present_flag = 1;
        }
        sps->vui.video_signal_type_present_flag = 1;
    }

    if (ctx->chroma_sample_loc_type >= 0) {
        sps->vui.chroma_sample_loc_type_top_field =
            ctx->chroma_sample_loc_type;
        sps->vui.chroma_sample_loc_type_bottom_field =
            ctx->chroma_sample_loc_type;
        sps->vui.chroma_loc_info_present_flag = 1;
        need_vui = 1;
    }

    if (ctx->tick_rate.num && ctx->tick_rate.den) {
        int num, den;

        av_reduce(&num, &den, ctx->tick_rate.num, ctx->tick_rate.den,
                  UINT32_MAX > INT_MAX ? UINT32_MAX : INT_MAX);

        sps->vui.time_scale        = num;
        sps->vui.num_units_in_tick = den;

        sps->vui.timing_info_present_flag = 1;
        need_vui = 1;
    }
    SET_VUI_FIELD(fixed_frame_rate_flag);
    if (ctx->zero_new_constraint_set_flags) {
        sps->constraint_set4_flag = 0;
        sps->constraint_set5_flag = 0;
    }

    if (sps->separate_colour_plane_flag || sps->chroma_format_idc == 0) {
        crop_unit_x = 1;
        crop_unit_y = 2 - sps->frame_mbs_only_flag;
    } else {
        crop_unit_x = 1 + (sps->chroma_format_idc < 3);
        crop_unit_y = (1 + (sps->chroma_format_idc < 2)) *
                       (2 - sps->frame_mbs_only_flag);
    }
#define CROP(border, unit) do { \
        if (ctx->crop_ ## border >= 0) { \
            if (ctx->crop_ ## border % unit != 0) { \
                av_log(bsf, AV_LOG_ERROR, "Invalid value for crop_%s: " \
                       "must be a multiple of %d.\n", #border, unit); \
                return AVERROR(EINVAL); \
            } \
            sps->frame_crop_ ## border ## _offset = \
                  ctx->crop_ ## border / unit; \
            sps->frame_cropping_flag = 1; \
        } \
    } while (0)
    CROP(left,   crop_unit_x);
    CROP(right,  crop_unit_x);
    CROP(top,    crop_unit_y);
    CROP(bottom, crop_unit_y);
#undef CROP

    if (ctx->level != LEVEL_UNSET) {
        int level_idc;

        if (ctx->level == LEVEL_AUTO) {
            const H264LevelDescriptor *desc;
            int64_t bit_rate;
            int width, height, dpb_frames;
            int framerate;

            if (sps->vui.nal_hrd_parameters_present_flag) {
                bit_rate = (sps->vui.nal_hrd_parameters.bit_rate_value_minus1[0] + 1) *
                    (INT64_C(1) << (sps->vui.nal_hrd_parameters.bit_rate_scale + 6));
            } else if (sps->vui.vcl_hrd_parameters_present_flag) {
                bit_rate = (sps->vui.vcl_hrd_parameters.bit_rate_value_minus1[0] + 1) *
                    (INT64_C(1) << (sps->vui.vcl_hrd_parameters.bit_rate_scale + 6));
                // Adjust for VCL vs. NAL limits.
                bit_rate = bit_rate * 6 / 5;
            } else {
                bit_rate = 0;
            }

            // Don't use max_dec_frame_buffering if it is only inferred.
            dpb_frames = sps->vui.bitstream_restriction_flag ?
                sps->vui.max_dec_frame_buffering : H264_MAX_DPB_FRAMES;

            width  = 16 * (sps->pic_width_in_mbs_minus1 + 1);
            height = 16 * (sps->pic_height_in_map_units_minus1 + 1) *
                (2 - sps->frame_mbs_only_flag);

            if (sps->vui.timing_info_present_flag)
                framerate = sps->vui.time_scale / sps->vui.num_units_in_tick / 2;
            else
                framerate = 0;

            desc = ff_h264_guess_level(sps->profile_idc, bit_rate, framerate,
                                       width, height, dpb_frames);
            if (desc) {
                if ((desc->level_idc == 11) && desc->constraint_set3_flag)
                    // This ensures that for level 1b the correct level
                    // will be inferred below.
                    level_idc = 9;
                else
                    level_idc = desc->level_idc;
            } else {
                av_log(bsf, AV_LOG_WARNING, "Stream does not appear to "
                       "conform to any level: using level 6.2.\n");
                level_idc = 62;
            }
        } else {
            level_idc = ctx->level;
        }

        if (level_idc == 9) {
            if (sps->profile_idc == 66 ||
                sps->profile_idc == 77 ||
                sps->profile_idc == 88) {
                sps->level_idc = 11;
                sps->constraint_set3_flag = 1;
            } else {
                sps->level_idc = 9;
            }
        } else {
            // If the earlier level was 1b or the new level
            // is in danger of being mistaken for 1b,
            // we clear constraint_set3_flag.
            if ((level_idc        == 11 ||
                 sps->level_idc   == 11) &&
                (sps->profile_idc == 66 ||
                 sps->profile_idc == 77 ||
                 sps->profile_idc == 88))
                sps->constraint_set3_flag = 0;
            sps->level_idc = level_idc;
        }
    }

    if (ctx->delete_filler & 2) {
        int err = ff_cbs_make_unit_writable(ctx->output, unit);
        if (err < 0)
            return err;
        sps = unit->content;

        sps->vui.nal_hrd_parameters_present_flag = 0;
        sps->vui.vcl_hrd_parameters_present_flag = 0;
        sps->vui.pic_struct_present_flag         = 0;
        sps->vui.low_delay_hrd_flag = 1 - sps->vui.fixed_frame_rate_flag;
    }
    if (sps->bit_depth_luma_minus8) {
        if (ctx->qp.qp.state > 0) {
            av_log(bsf, AV_LOG_ERROR, "Multiple qp values with bit-depth "
                                      "different from 8 are not supported.\n");
            return AVERROR(EINVAL);
        }

        if (ctx->qp_histogram) {
            av_log(bsf, AV_LOG_VERBOSE, "qp-statistics unavailable with "
                                        "bit-depth > 8.\n");
            ctx->qp_histogram -= 26;
            av_freep(&ctx->qp_histogram);
        }
    }

    if (ctx->reorder_frames >= 0) {
        int max_dpb = ff_cbs_h264_get_max_dpb_frames(sps);

        if (ctx->reorder_frames > max_dpb) {
            av_log(bsf, AV_LOG_ERROR, "The specified value for reorder_frames "
                   "is incompatible with profile and level restrictions. The "
                   "maximal possible value is %d.\n", max_dpb);
            return AVERROR(EINVAL);
        }

        sps->vui.max_num_reorder_frames  = ctx->reorder_frames;
        sps->vui.max_dec_frame_buffering = FFMAX(ctx->reorder_frames,
                                                 sps->vui.max_dec_frame_buffering);
        sps->vui.bitstream_restriction_flag = 1;
        need_vui = 1;
    }

    if (need_vui)
        sps->vui_parameters_present_flag = 1;

    return 0;
}

static int h264_metadata_update_pps(AVBSFContext *bsf,
                                    CodedBitstreamUnit *unit)
{
    H264MetadataContext *ctx = bsf->priv_data;
    CodedBitstreamH264Context *h264_in = ctx->input->priv_data;
    H264RawPPS *pps = unit->content;
    H264RawSPS *sps = h264_in->sps[pps->seq_parameter_set_id];
    int err;

    if (ctx->misc.misc.interlaced && sps->frame_mbs_only_flag &&
        pps->bottom_field_pic_order_in_frame_present_flag) {
        err = ff_cbs_make_unit_writable(ctx->output, unit);
        if (err < 0)
            return err;
        pps = unit->content;

        pps->bottom_field_pic_order_in_frame_present_flag = 0;
        ctx->misc.misc.flags |= PROGRESSIVE;
    }

    if (ctx->ref.ref.state & 6) {
        err = ff_cbs_make_unit_writable(ctx->output, unit);
        if (err < 0)
            return err;
        pps = unit->content;

        if (ctx->ref.ref.state & 2)
            pps->num_ref_idx_l0_default_active_minus1 = ctx->ref.ref.ref[0];
        if (ctx->ref.ref.state & 4)
            pps->num_ref_idx_l1_default_active_minus1 = ctx->ref.ref.ref[1];
    }

    if (ctx->qp.qp.state) {
        if (ctx->qp.qp.state > 0 && pps->pic_parameter_set_id) {
            av_log(bsf, AV_LOG_ERROR, "Multiple qp values only allowed when "
                                      "the only input PPS_id is 0.\n");
            return AVERROR(EINVAL);
        }

        err = ff_cbs_make_unit_writable(ctx->output, unit);
        if (err < 0)
            return err;
        pps = unit->content;

        pps->pic_init_qp_minus26 = ctx->qp.qp.qp_val[0];
        if (ctx->qp.qp.weighted_pred_flag)
            pps->weighted_pred_flag = 1;
    }

    return 0;
}

static int h264_metadata_write_ps(CodedBitstreamContext *ctx,
                                  CodedBitstreamFragment *au, int max_ps,
                                  int *position, PSPack *ps, uint8_t *status,
                                  CodedBitstreamUnitType type)
{
    int err, pos = *position;

    for (int i = 0; i <= max_ps; i++) {
        if (status[i] & WRITE) {
            err = ff_cbs_insert_unit_content(au, pos++, type,
                                             ps[i].ps, NULL);
            if (err < 0)
                return err;

            status[i] = (status[i] & ~(DIFFERS_OUT_EFF|WRITE)) | WRITTEN_THIS_GOP;
        }
    }

    *position = pos;

    return 0;
}

static int h264_metadata_check_status(uint8_t *status, int count, int flag)
{
    for (int i = 0; i <= count; i++) {
        if (status[i] & IS_IN_EXTRA &&
           (status[i] & DIFFERS_OUT_EFF ||
           (status[i] & (flag|WRITTEN_THIS_GOP)) == flag))
            return 0;
    }

    return 1;
}

static int h264_metadata_handle_ps(AVBSFContext *bsf,
                                   CodedBitstreamFragment *au,
                                   int keyframe, int has_ps, int forbid_mod)
{
    H264MetadataContext *ctx = bsf->priv_data;
    CodedBitstreamH264Context *out = ctx->output->priv_data;
    Extradata *extra = ctx->extra;
    Header   *header = ctx->header;
    int err, write = 0, mode = ctx->extradata;

    if (has_ps) {
        // In some cases, the flags usually determined by comparison are either
        // irrelevant for whether a PS will be written or not (PS in keyframes
        // in an INSERT mode as the right PS will be written anyway at the latest
        // when the PS becomes active the first time in a given GOP (if at all)).
        // Therefore sometimes some checks are omitted if they are unnecessary.
        // Notice that this implies that in an INSERT mode the DIFFERS_OUT flag
        // is only valid after the PS has been written in a GOP.
        int check = !(keyframe == 2 || keyframe && mode != MINIMIZE && !forbid_mod);

        for (int i = 0; i < au->nb_units; i++) {
            CodedBitstreamUnit *unit = &au->units[i];

            if (unit->type == H264_NAL_SPS || unit->type == H264_NAL_PPS) {
                PSPack *ps;
                int *max, is_sps;
                uint8_t id;

                if (unit->type == H264_NAL_SPS) {
                    id     = ((H264RawSPS*)unit->content)->seq_parameter_set_id;
                    ps     = &extra->sps[id];

                    extra->sps_status[id] |= IS_IN_EXTRA;

                    max    = &extra->max_sps;
                    is_sps = 1;
                } else {
                    id     = ((H264RawPPS*)unit->content)->pic_parameter_set_id;
                    ps     = &extra->pps[id];

                    extra->pps_status[id] |= IS_IN_EXTRA;

                    max    = &extra->max_pps;
                    is_sps = 0;
                }

                *max = FFMAX(*max, id);

                av_buffer_unref(&ps->ps_ref);

                ps->ps     = unit->content;
                ps->ps_ref = unit->content_ref;

                unit->content_ref = NULL;
                ff_cbs_delete_unit(au, i);
                i--;

                if (check) {
                    int res;
                    uint8_t *status = is_sps ? &extra->sps_status[id] :
                                               &extra->pps_status[id];

                    res = compare_ps(ps->ps, is_sps ? (void*)out->sps[id] :
                                              (void*)out->pps[id], is_sps);

                    if (!res) {
                        // If the following is true, then the new PS in extra
                        // and the old PS in extra agree (as they are equal to
                        // the currently visible PS in the output context) so
                        // that no flags need to be updated.
                        if (!(*status & DIFFERS_OUT))
                            continue;
                    } else if (forbid_mod) {
                        av_log(bsf, AV_LOG_ERROR, "Extradata changed. Output"
                                                         " likely corrupt.\n");
                        err = AVERROR_INVALIDDATA;
                        goto fail;
                    } else {
                        res = DIFFERS_OUT;
                        av_log(bsf, AV_LOG_VERBOSE, "%cPS with id %"PRIu8" "
                               "likely changed\n", is_sps ? 'S' : 'P', id);
                    }

                    *status = (*status & ~DIFFERS_OUT) | res;

                    if (header) {
                        res = compare_ps(ps->ps, is_sps ? header->sps[id].ps :
                                                  header->pps[id].ps, is_sps);

                        res = res ? DIFFERS_HEADER : 0;

                        *status = (*status & ~DIFFERS_HEADER) | res;
                    }
                }
            } else if (unit->type == H264_NAL_SPS_EXT) {
                av_log(bsf, AV_LOG_ERROR, "Sequence parameter set extension "
                                          "not supported.\n");
                return AVERROR_PATCHWELCOME;
            }
        }
    }

    // In a new GOP, the WRITTEN_THIS_GOP flags need to be reset.
    // (Except in INSERT mode, as all currently available extradata
    // from extra is written anyway in every keyframe.)
    if (keyframe == 1 && mode != INSERT) {
        for (int i = extra->max_sps; i >= 0; i--)
            extra->sps_status[i] &= ~WRITTEN_THIS_GOP;
        for (int i = extra->max_pps; i >= 0; i--)
            extra->pps_status[i] &= ~WRITTEN_THIS_GOP;
    }

    if (keyframe == 2 || mode == INSERT && keyframe) {
        write = 1;

        for (int i = extra->max_sps; i >= 0; i--)
            if (extra->sps_status[i] & IS_IN_EXTRA)
                extra->sps_status[i] |= WRITE;
        for (int i = extra->max_pps; i >= 0; i--)
            if (extra->pps_status[i] & IS_IN_EXTRA)
                extra->pps_status[i] |= WRITE;
    } else {
        int i;
        uint8_t sps_id;

        for (i = 0; i < au->nb_units; i++) {
            CodedBitstreamUnit *unit = &au->units[i];

            if (unit->type == H264_NAL_SLICE     ||
                unit->type == H264_NAL_IDR_SLICE ||
                unit->type == H264_NAL_AUXILIARY_SLICE) {
                H264RawSliceHeader *slice = unit->content;

                sps_id = ((H264RawPPS*)extra->pps[slice->pic_parameter_set_id].ps)
                                            ->seq_parameter_set_id;
                break;
            }
        }

        if (i == au->nb_units) {
            av_log(bsf, AV_LOG_ERROR, "No slice in access unit.\n");
            return AVERROR_INVALIDDATA;
        }

        if (mode == INSERT_2 && keyframe) {
            extra->sps_status[sps_id] |= WRITE;
            write = 1;

            for (i = 0; i <= extra->max_pps; i++) {
                if (extra->pps[i].ps &&
                   ((H264RawPPS*)extra->pps[i].ps)->seq_parameter_set_id == sps_id)
                   extra->pps_status[i] |= WRITE;
            }
        } else {
            int flag = mode == MINIMIZE ? DIFFERS_HEADER : 0;

            if (extra->sps_status[sps_id] & DIFFERS_OUT ||
               (extra->sps_status[sps_id] & (flag|WRITTEN_THIS_GOP)) == flag) {
                extra->sps_status[sps_id] |= WRITE;
                write = 1;

                // A PPS whose underlying SPS has been modified is treated as modified.
                if (!mode || !keyframe) {
                    for (int i = 0; i <= extra->max_pps; i++) {
                        H264RawPPS *pps = extra->pps[i].ps;

                        if (pps && pps->seq_parameter_set_id == sps_id)
                            extra->pps_status[i] |= UNWRITTEN_AFTER_SPS_CHANGE;
                    }
                }
            }

            for (; i < au->nb_units; i++) {
                CodedBitstreamUnit *unit = &au->units[i];

                if (unit->type == H264_NAL_SLICE     ||
                    unit->type == H264_NAL_IDR_SLICE ||
                    unit->type == H264_NAL_AUXILIARY_SLICE) {
                    H264RawSliceHeader *slice = unit->content;
                    uint8_t pps_id = slice->pic_parameter_set_id;

                    if (extra->pps_status[pps_id] & DIFFERS_OUT_EFF ||
                       (extra->pps_status[pps_id] & (flag|WRITTEN_THIS_GOP)) == flag) {
                        extra->pps_status[pps_id] |= WRITE;
                        write = 1;
                    }
                }
            }
        }
    }

    if (write) {
        int position;

        for (position = 0; position < au->nb_units; position++) {
            if (au->units[position].type != H264_NAL_AUD)
                break;
        }

        // All PS marked as WRITE are now written and their flags updated.
        err = h264_metadata_write_ps(ctx->output, au, extra->max_sps, &position,
                                     extra->sps, extra->sps_status, H264_NAL_SPS);
        if (err < 0)
            goto fail;

        err = h264_metadata_write_ps(ctx->output, au, extra->max_pps, &position,
                                     extra->pps, extra->pps_status, H264_NAL_PPS);
        if (err < 0)
            goto fail;
    }

    // "Normal" access units (i.e. non-keyframes that don't contain
    // parameter sets) need only be looked at if there is currently
    // a parameter set that would need to be written if it were used
    // in a future access unit. Here we check whether this is so.
    if (keyframe == 1 || has_ps || write) {
        int up_to_date, flag = mode == MINIMIZE ? DIFFERS_HEADER : 0;

        up_to_date = h264_metadata_check_status(extra->sps_status,
                                                extra->max_sps, flag);

        if (up_to_date)
            up_to_date = h264_metadata_check_status(extra->pps_status,
                                                    extra->max_pps, flag);

        ctx->extra_status = up_to_date;
    }

    return 0;

fail:
    ctx->extradata = PASS;

    return err;
}

static int h264_metadata_update_side_data(AVBSFContext *bsf, AVPacket *pkt)
{
    H264MetadataContext *ctx = bsf->priv_data;
    CodedBitstreamFragment *au = &ctx->access_unit;
    uint8_t *side_data;
    size_t side_data_size;
    int err, i;

    side_data = av_packet_get_side_data(pkt, AV_PKT_DATA_NEW_EXTRADATA,
                                        &side_data_size);
    if (!side_data_size)
        return 0;

    err = ff_cbs_read(ctx->input, au, side_data, side_data_size);
    if (err < 0) {
        av_log(bsf, AV_LOG_ERROR, "Failed to read extradata from packet side data.\n");
        return err;
    }

    for (i = 0; i < au->nb_units; i++) {
        if (au->units[i].type == H264_NAL_SPS) {
            err = h264_metadata_update_sps(bsf, &au->units[i]);
            if (err < 0)
                return err;
        }
    }

    err = ff_cbs_write_fragment_data(ctx->output, au);
    if (err < 0) {
        av_log(bsf, AV_LOG_ERROR, "Failed to write extradata into packet side data.\n");
        return err;
    }

    side_data = av_packet_new_side_data(pkt, AV_PKT_DATA_NEW_EXTRADATA, au->data_size);
    if (!side_data)
        return AVERROR(ENOMEM);
    memcpy(side_data, au->data, au->data_size);

    ff_cbs_fragment_reset(au);

    return 0;
}

static int h264_metadata_handle_display_orientation(AVBSFContext *bsf,
                                                    AVPacket *pkt,
                                                    CodedBitstreamFragment *au,
                                                    int seek_point)
{
    H264MetadataContext *ctx = bsf->priv_data;
    SEIRawMessage *message;
    int err;

    message = NULL;
    while (ff_cbs_sei_find_message(ctx->output, au,
                                   SEI_TYPE_DISPLAY_ORIENTATION,
                                   &message) == 0) {
        H264RawSEIDisplayOrientation *disp = message->payload;
        int32_t *matrix;

        matrix = av_malloc(9 * sizeof(int32_t));
        if (!matrix)
            return AVERROR(ENOMEM);

        av_display_rotation_set(matrix,
                                disp->anticlockwise_rotation *
                                180.0 / 65536.0);
        av_display_matrix_flip(matrix, disp->hor_flip, disp->ver_flip);

        // If there are multiple display orientation messages in an
        // access unit, then the last one added to the packet (i.e.
        // the first one in the access unit) will prevail.
        err = av_packet_add_side_data(pkt, AV_PKT_DATA_DISPLAYMATRIX,
                                      (uint8_t*)matrix,
                                      9 * sizeof(int32_t));
        if (err < 0) {
            av_log(bsf, AV_LOG_ERROR, "Failed to attach extracted "
                   "displaymatrix side data to packet.\n");
            av_free(matrix);
            return AVERROR(ENOMEM);
        }
    }

    if (ctx->display_orientation == REMOVE ||
        ctx->display_orientation == INSERT) {
        ff_cbs_sei_delete_message_type(ctx->output, au,
                                       SEI_TYPE_DISPLAY_ORIENTATION);
    }

    if (ctx->display_orientation == INSERT) {
        H264RawSEIDisplayOrientation *disp =
            &ctx->display_orientation_payload;
        uint8_t *data;
        size_t size;
        int write = 0;

        data = av_packet_get_side_data(pkt, AV_PKT_DATA_DISPLAYMATRIX, &size);
        if (data && size >= 9 * sizeof(int32_t)) {
            int32_t matrix[9];
            double dmatrix[9];
            int hflip, vflip, i;
            double scale_x, scale_y, angle;

            memcpy(matrix, data, sizeof(matrix));

            for (i = 0; i < 9; i++)
                dmatrix[i] = matrix[i] / 65536.0;

            // Extract scale factors.
            scale_x = hypot(dmatrix[0], dmatrix[3]);
            scale_y = hypot(dmatrix[1], dmatrix[4]);

            // Select flips to make the main diagonal positive.
            hflip = dmatrix[0] < 0.0;
            vflip = dmatrix[4] < 0.0;
            if (hflip)
                scale_x = -scale_x;
            if (vflip)
                scale_y = -scale_y;

            // Rescale.
            for (i = 0; i < 9; i += 3) {
                dmatrix[i]     /= scale_x;
                dmatrix[i + 1] /= scale_y;
            }

            // Extract rotation.
            angle = atan2(dmatrix[3], dmatrix[0]);

            if (!(angle >= -M_PI && angle <= M_PI) ||
                matrix[2] != 0.0 || matrix[5] != 0.0 ||
                matrix[6] != 0.0 || matrix[7] != 0.0) {
                av_log(bsf, AV_LOG_WARNING, "Input display matrix is not "
                       "representable in H.264 parameters.\n");
            } else {
                disp->hor_flip = hflip;
                disp->ver_flip = vflip;
                disp->anticlockwise_rotation =
                    (uint16_t)rint((angle >= 0.0 ? angle
                                                 : angle + 2 * M_PI) *
                                   32768.0 / M_PI);
                write = 1;
            }
        }

        if (seek_point) {
            if (!isnan(ctx->rotate)) {
                disp->anticlockwise_rotation =
                    (uint16_t)rint((ctx->rotate >= 0.0 ? ctx->rotate
                                                       : ctx->rotate + 360.0) *
                                   65536.0 / 360.0);
                write = 1;
            }
            if (ctx->flip) {
                disp->hor_flip = !!(ctx->flip & FLIP_HORIZONTAL);
                disp->ver_flip = !!(ctx->flip & FLIP_VERTICAL);
                write = 1;
            }
        }

        if (write) {
            disp->display_orientation_repetition_period = 1;

            err = ff_cbs_sei_add_message(ctx->output, au, 1,
                                         SEI_TYPE_DISPLAY_ORIENTATION,
                                         disp, NULL);
            if (err < 0) {
                av_log(bsf, AV_LOG_ERROR, "Failed to add display orientation "
                       "SEI message to access unit.\n");
                return err;
            }
        }
    }

    return 0;
}

static int h264_metadata_filter(AVBSFContext *bsf, AVPacket *pkt)
{
    H264MetadataContext *ctx = bsf->priv_data;
    CodedBitstreamH264Context *h264_in = ctx->input->priv_data;
    CodedBitstreamFragment *au = &ctx->access_unit;
    int err, i, has_ps, seek_point, disposable;

    err = ff_bsf_get_packet_ref(bsf, pkt);
    if (err < 0) {
        if (err == AVERROR_EOF && ctx->header_unused > 0) {
            ctx->header_unused = 0;

            if (h264_metadata_update_extradata(bsf, pkt) >= 0)
                err = 0;

            goto fail;
        }
        return err;
    }

    err = h264_metadata_update_side_data(bsf, pkt);
    if (err < 0)
        goto fail;

    err = ff_cbs_read_packet(ctx->input, au, pkt);
    if (err < 0) {
        av_log(bsf, AV_LOG_ERROR, "Failed to read packet.\n");
        goto fail;
    }

    if (au->nb_units == 0) {
        av_log(bsf, AV_LOG_ERROR, "No NAL units in packet.\n");
        err = AVERROR_INVALIDDATA;
        goto fail;
    }

    // If an AUD is present, it must be the first NAL unit.
    if (au->units[0].type == H264_NAL_AUD) {
        if (ctx->aud == REMOVE)
            ff_cbs_delete_unit(au, 0);
    } else {
        if (ctx->aud == INSERT) {
            err = h264_metadata_insert_aud(bsf, au);
            if (err < 0)
                goto fail;
        }
    }

    has_ps = 0;
    for (i = 0; i < au->nb_units; i++) {
        if (au->units[i].type == H264_NAL_SPS) {
            err = h264_metadata_update_sps(bsf, &au->units[i]);
            if (err < 0)
                goto fail;
            has_ps |= 1;
        }

        if (au->units[i].type == H264_NAL_PPS) {
            err = h264_metadata_update_pps(bsf, &au->units[i]);
            if (err < 0)
                goto fail;
            has_ps |= 2;
        }
    }

    // The current packet should be treated as a seek point for metadata
    // insertion if any of:
    // - It is the first packet in the stream.
    // - It contains an SPS, indicating that a sequence might start here.
    // - It is marked as containing a key frame.
    seek_point = !ctx->done_first_au || has_ps & 1 ||
        (pkt->flags & AV_PKT_FLAG_KEY);

    if (ctx->sei_user_data && seek_point) {
        err = ff_cbs_sei_add_message(ctx->output, au, 1,
                                     SEI_TYPE_USER_DATA_UNREGISTERED,
                                     &ctx->sei_user_data_payload, NULL);
        if (err < 0) {
            av_log(bsf, AV_LOG_ERROR, "Failed to add user data SEI "
                   "message to access unit.\n");
            goto fail;
        }
    }

    if (ctx->delete_filler) {
        int idr_access = h264_in->last_slice_nal_unit_type == H264_NAL_IDR_SLICE;

        for (i = au->nb_units - 1; i >= 0; i--) {
            if (au->units[i].type == H264_NAL_FILLER_DATA) {
                ff_cbs_delete_unit(au, i);
                continue;
            }
            if (au->units[i].type == H264_NAL_SEI) {
                H264RawSEI *sei = au->units[i].content;
                SEIRawMessageList *list = &sei->message_list;

                for (int j = list->nb_messages - 1; j >= 0; j--) {
                    SEIRawMessage *message = &list->messages[j];

                    if (ctx->delete_filler == 1 &&
                        message->payload_type != SEI_TYPE_FILLER_PAYLOAD)
                        continue;

                    if (message->payload_type == SEI_TYPE_RECOVERY_POINT
                                && !idr_access)
                        continue;

                    if (message->payload_type ==
                                SEI_TYPE_USER_DATA_UNREGISTERED) {
                        SEIRawUserDataUnregistered *user_data = message->payload;
                        if (user_data->data_length >= 4 &&
                            !memcmp("x264", user_data->data, 4))
                            continue;
                    }

                    ff_cbs_sei_delete_message(ctx->output, au, &au->units[i], j);
                }
            }
        }
    }

    if (ctx->misc.misc.flags) {
        int idr_access = h264_in->last_slice_nal_unit_type == H264_NAL_IDR_SLICE;
        int mmco = 0;
        uint8_t flags = ctx->misc.misc.flags;

        for (i = 0; i < au->nb_units; i++) {
            CodedBitstreamUnit    *unit = &au->units[i];
            CodedBitstreamUnitType type = unit->type;
            H264RawSliceHeader *slice;
            H264RawPPS *pps;
            int idr_slice;

            if (type != H264_NAL_SLICE && type != H264_NAL_IDR_SLICE
                                       && type != H264_NAL_AUXILIARY_SLICE)
                continue;

            slice     = unit->content;
            pps       = h264_in->pps[slice->pic_parameter_set_id];
            idr_slice = type == H264_NAL_IDR_SLICE ||
                        type == H264_NAL_AUXILIARY_SLICE && idr_access;

            if (idr_slice && flags & MIN_IDR)
                slice->idr_pic_id = idr_access & ctx->misc.misc.idr; /* & same as && */

            if (flags & MIN_SLICE_TYPE && slice->slice_type >= 5)
                slice->slice_type -= 5;

            if (flags & MOD_REF && !idr_slice) {
                int slice_type, slice_type_p, slice_type_b;
                slice_type = slice->slice_type;
                if (slice_type >= 5)
                    slice_type -= 5;
                slice_type_p  = slice_type == 0 || slice_type == 3;
                slice_type_b  = slice_type == 1;
                slice->num_ref_idx_active_override_flag = 0;

                if (slice_type_p || slice_type_b) {
                    uint8_t ref_default = ctx->ref.ref.ref[0];
                    ref_default = ctx->ref.ref.state & 2 ? ref_default :
                                     pps->num_ref_idx_l0_default_active_minus1;

                    if (ref_default != slice->num_ref_idx_l0_active_minus1 ||
                        (ref_default > 15 && !slice->field_pic_flag))
                        slice->num_ref_idx_active_override_flag = 1;
                    else if (slice_type_b) {
                        ref_default = ctx->ref.ref.ref[1];
                        ref_default = ctx->ref.ref.state & 4 ? ref_default :
                                      pps->num_ref_idx_l1_default_active_minus1;

                        if (ref_default != slice->num_ref_idx_l1_active_minus1 ||
                            (ref_default > 15 && !slice->field_pic_flag))
                            slice->num_ref_idx_active_override_flag = 1;
                    }
                }
            }

            if (flags & (PROGRESSIVE|MOD_FRAME_NUM|MOD_POC)
                && h264_in->active_sps->pic_order_cnt_type == 0) {
                const H264RawSPS *sps = h264_in->active_sps;
                uint16_t poc_max = (1 << (sps->log2_max_pic_order_cnt_lsb_minus4 + 4)) - 1;

                if (flags & PROGRESSIVE && sps->frame_mbs_only_flag &&
                    pps->bottom_field_pic_order_in_frame_present_flag) {
                    if (slice->delta_pic_order_cnt_bottom < 0)
                        slice->pic_order_cnt_lsb += slice->delta_pic_order_cnt_bottom;
                    slice->delta_pic_order_cnt_bottom = 0;
                }

                if (flags & (MOD_FRAME_NUM|MOD_POC) && !idr_slice) {
                    uint16_t frame_num_max = (1 << (sps->log2_max_frame_num_minus4 + 4)) - 1;

                    if (ctx->frame_num_offset == 65536) {
                        if (slice->frame_num) {
                            ctx->frame_num_offset = -slice->frame_num;
                        } else {
                            flags                &= ~MOD_FRAME_NUM;
                            ctx->misc.misc.flags  = flags;
                        }
                    }
                    if (ctx->poc_offset       == 65536) {
                        if (slice->pic_order_cnt_lsb) {
                            ctx->poc_offset       = -slice->pic_order_cnt_lsb;
                        } else {
                            flags                &= ~MOD_POC;
                            ctx->misc.misc.flags  = flags;
                        }
                    }

                    slice->pic_order_cnt_lsb += ctx->poc_offset;
                    slice->frame_num         += ctx->frame_num_offset;
                    slice->frame_num         &= frame_num_max;

                    // Look for memory_management_control_operation equal to 5
                    if (!mmco && slice->adaptive_ref_pic_marking_mode_flag) {
                        for (int j = 0; slice->mmco[j].memory_management_control_operation; j++) {
                            if (slice->mmco[j].memory_management_control_operation == 5) {
                                mmco = 1;
                                if (slice->frame_num == 1 &
                                   !slice->redundant_pic_cnt) {
                                    av_log(bsf, AV_LOG_WARNING,
                                           "Primary picture with mmco equal to"
                                           "5 has frame_num 1 post offsetting."
                                           "Track might be out-of-spec.\n");
                                } else {
                                    av_log(bsf, AV_LOG_VERBOSE, "Found mmco "
                                           "equal to 5.\n");
                                }
                                break;
                            }
                        }
                    }
                }

                slice->pic_order_cnt_lsb &= poc_max;
            }

            if (flags & SINGLE_QP) {
                int qp = pps->pic_init_qp_minus26 + slice->slice_qp_delta;

                if (ctx->qp_histogram)
                    ctx->qp_histogram[qp]++;

                slice->slice_qp_delta += pps->pic_init_qp_minus26
                                       - ctx->qp.qp.qp_val[0];
            }
        }

        ctx->misc.misc.idr = idr_access & !ctx->misc.misc.idr; /* & same as && */
        if (mmco || idr_access) {
            ctx->misc.misc.flags &= ~(MOD_FRAME_NUM|MOD_POC);
        }
    }

    if (ctx->qp.qp.state > 0) {
        const H264RawPPS *pps = h264_in->active_pps;
        int qp = 100, pps_id, qp_delta;

        for (i = 0; i < au->nb_units; i++) {
            const CodedBitstreamUnit *unit = &au->units[i];
            CodedBitstreamUnitType    type = unit->type;

            if (type == H264_NAL_SLICE || type == H264_NAL_IDR_SLICE
                                       || type == H264_NAL_AUXILIARY_SLICE) {
                const H264RawSliceHeader *slice = unit->content;

                if (qp != 100 && slice->slice_qp_delta != qp)
                    break;

                qp = slice->slice_qp_delta;
            }
        }

        if (qp == 100) {
            av_log(bsf, AV_LOG_ERROR, "No slice in access unit.\n");
            err = AVERROR(EINVAL);
            goto fail;
        }

        if (i == au->nb_units) {
            qp    += pps->pic_init_qp_minus26;
            pps_id = ctx->qp_lut[qp];
        } else {
            int cost[5] = { 0 }, min, k = ctx->qp.qp.num_qp_minus_1;

            av_log(bsf, AV_LOG_DEBUG, "Found access unit with slices with "
                                      "different slice_qp_delta values.\n");

            #define GOL_DIFF(i,j) (i == j ? -1 : av_log2(FFABS(i - j)))
            for (i = 0; i < au->nb_units; i++) {
                const CodedBitstreamUnit *unit = &au->units[i];
                CodedBitstreamUnitType    type = unit->type;

                if (type == H264_NAL_SLICE || type == H264_NAL_IDR_SLICE
                                           || type == H264_NAL_AUXILIARY_SLICE) {
                    const H264RawSliceHeader *slice = unit->content;

                    for (int j = k; j >= 0; j--)
                        cost[j] += GOL_DIFF(slice->slice_qp_delta, ctx->qp.qp.qp_val[j]) + (j + 1) / 2;
                }
            }

            pps_id = 0;
            min    = cost[0];
            for (int j = 1; j <= k; j++) {
                if (cost[j] < min) {
                    pps_id = j;
                    min    = cost[j];
                }
            }
        }

        qp_delta = pps->pic_init_qp_minus26 - ctx->qp.qp.qp_val[pps_id];

        for (i = 0; i < au->nb_units; i++) {
            CodedBitstreamUnit    *unit = &au->units[i];
            CodedBitstreamUnitType type = unit->type;

            if (type == H264_NAL_SLICE || type == H264_NAL_IDR_SLICE
                                       || type == H264_NAL_AUXILIARY_SLICE) {
                H264RawSliceHeader *slice = unit->content;

                slice->slice_qp_delta      += qp_delta;
                slice->pic_parameter_set_id = pps_id;

                if (ctx->qp_histogram)
                    ctx->qp_histogram[slice->slice_qp_delta + ctx->qp.qp.qp_val[pps_id]]++;
            }
        }
    }

    if (ctx->extradata != PASS) {
        if (ctx->extradata == REMOVE) {
            if (has_ps) {
                for (int i = au->nb_units; i >= 0; i--) {
                    if (au->units[i].type == H264_NAL_SPS ||
                        au->units[i].type == H264_NAL_PPS) {
                        ff_cbs_delete_unit(au, i);
                    }
                }
            }
        } else {
            if (!ctx->extra_status || has_ps ||
                   pkt->flags & AV_PKT_FLAG_KEY || !ctx->done_first_au) {
                err = h264_metadata_handle_ps(bsf, au, pkt->flags & AV_PKT_FLAG_KEY
                                                   || !ctx->done_first_au, has_ps,
                                                        ctx->qp.qp.num_qp_minus_1);
                if (err < 0)
                    goto fail;
            }

            if (ctx->header_unused) {
                Extradata *extra = ctx->extra;
                uint8_t id = h264_in->active_sps->seq_parameter_set_id;

                if (!ctx->dts)
                    ctx->last_dts = pkt->dts;

                if ((extra->sps_status[id] & HEADER_FIRST_USE) == IS_IN_HEADER) {
                    extra->sps_status[id] |= HEADER_WAS_USED;
                    ctx->header_unused--;
                }
                for (int i = 0; i < au->nb_units; i++) {
                    CodedBitstreamUnit *unit = &au->units[i];

                    if (unit->type == H264_NAL_SLICE     ||
                        unit->type == H264_NAL_IDR_SLICE ||
                        unit->type == H264_NAL_AUXILIARY_SLICE) {
                        H264RawSliceHeader *slice = unit->content;
                        id = slice->pic_parameter_set_id;

                        if ((extra->pps_status[id] & HEADER_FIRST_USE) == IS_IN_HEADER) {
                            extra->pps_status[id] |= HEADER_WAS_USED;
                            ctx->header_unused--;
                        }
                    }
                }
            }
        }
    }

    if (ctx->display_orientation != PASS) {
        err = h264_metadata_handle_display_orientation(bsf, pkt, au,
                                                       seek_point);
        if (err < 0)
            goto fail;
    }

    err = ff_cbs_write_packet(ctx->output, pkt, au);
    if (err < 0) {
        av_log(bsf, AV_LOG_ERROR, "Failed to write packet.\n");
        goto fail;
    }

    disposable = AV_PKT_FLAG_DISPOSABLE;
    for (int i = 0; i < au->nb_units; i++) {
        const CodedBitstreamUnit *unit = &au->units[i];
        if ((unit->content && ((H264RawNALUnitHeader*)unit->content)->nal_ref_idc)
            || (!unit->content && unit->data[0] & 0x60)) {
            disposable = 0;
            break;
        }
    }
    pkt->flags = (pkt->flags & ~AV_PKT_FLAG_DISPOSABLE) | disposable;

    if (ctx->dts) {
        if (pkt->dts != AV_NOPTS_VALUE) {
            pkt->dts += ctx->dts;

            if (ctx->done_first_au)
                pkt->dts  = FFMAX(pkt->dts, ctx->last_dts);
            ctx->last_dts = pkt->dts;

            if (pkt->pts != AV_NOPTS_VALUE && pkt->dts > pkt->pts)
                av_log(bsf, AV_LOG_WARNING, "dts %"PRId64" still bigger "
                       "than pts %"PRId64".\n", pkt->dts, pkt->pts);
        }
    }

    ctx->done_first_au = 1;

    err = 0;
fail:
    ff_cbs_fragment_reset(au);

    if (err < 0)
        av_packet_unref(pkt);

    return err;
}

static int h264_metadata_alloc_ps(AVBSFContext *bsf, int header)
{
    H264MetadataContext *ctx = bsf->priv_data;

    ctx->extra = av_mallocz(sizeof(*ctx->extra));
    if (!ctx->extra)
        return AVERROR(ENOMEM);

    ctx->extra->max_sps = ctx->extra->max_pps = -1;

    if (header) {
        ctx->header = av_mallocz(sizeof(*ctx->header));
        if (!ctx->header)
            return AVERROR(ENOMEM);
    }

    return 0;
}

static int h264_metadata_prepare_header(AVBSFContext *bsf)
{
    H264MetadataContext *ctx = bsf->priv_data;
    Extradata *extra = ctx->extra;
    Header   *header = ctx->header;
    int num = 0;

    header->max_sps = extra->max_sps;
    header->max_pps = extra->max_pps;

    for (int i = 0; i <= extra->max_sps; i++) {
        if (extra->sps[i].ps) {
            header->sps[i].ps     = extra->sps[i].ps;
            header->sps[i].ps_ref = av_buffer_ref(extra->sps[i].ps_ref);
            if (!header->sps[i].ps_ref)
                return AVERROR(ENOMEM);
            extra->sps_status[i] |= IS_IN_HEADER;
            num++;
        }
    }

    for (int i = 0; i <= extra->max_pps; i++) {
        if (extra->pps[i].ps) {
            header->pps[i].ps     = extra->pps[i].ps;
            header->pps[i].ps_ref = av_buffer_ref(extra->pps[i].ps_ref);
            if (!header->pps[i].ps_ref)
                return AVERROR(ENOMEM);
            extra->pps_status[i] |= IS_IN_HEADER;
            num++;
        }
    }

    if (ctx->header_unused)
        ctx->header_unused = num;

    return 0;
}

static int h264_metadata_init(AVBSFContext *bsf)
{
    H264MetadataContext *ctx = bsf->priv_data;
    CodedBitstreamFragment *au = &ctx->access_unit;
    int err, i, flags;

    if (ctx->sei_user_data) {
        SEIRawUserDataUnregistered *udu = &ctx->sei_user_data_payload;
        int j;

        // Parse UUID.  It must be a hex string of length 32, possibly
        // containing '-'s between hex digits (which we ignore).
        for (i = j = 0; j < 32 && i < 64 && ctx->sei_user_data[i]; i++) {
            int c, v;
            c = ctx->sei_user_data[i];
            if (c == '-') {
                continue;
            } else if (av_isxdigit(c)) {
                c = av_tolower(c);
                v = (c <= '9' ? c - '0' : c - 'a' + 10);
            } else {
                break;
            }
            if (j & 1)
                udu->uuid_iso_iec_11578[j / 2] |= v;
            else
                udu->uuid_iso_iec_11578[j / 2] = v << 4;
            ++j;
        }
        if (j == 32 && ctx->sei_user_data[i] == '+') {
            udu->data = (uint8_t*)ctx->sei_user_data + i + 1;
            udu->data_length = strlen(udu->data) + 1;
        } else {
            av_log(bsf, AV_LOG_ERROR, "Invalid user data: "
                   "must be \"UUID+string\".\n");
            err = AVERROR(EINVAL);
            goto fail;
        }
    }
    #define SET_CONDITIONALLY(option, default, value) do { \
        if (ctx->option == default) \
            ctx->option = value; \
    } while(0)
    if (ctx->preset == PASSTHROUGH_PRESET) {
        SET_CONDITIONALLY(aud,       UNDETERMINED, PASS);
        SET_CONDITIONALLY(extradata, UNDETERMINED, PASS);
        SET_CONDITIONALLY(delete_filler, -1, 0);
        SET_CONDITIONALLY(ref.init,      -1, 0);
        SET_CONDITIONALLY(misc.init,     -1, 0);
        SET_CONDITIONALLY(qp.init,       -1, 0);
    } else {
        SET_CONDITIONALLY(aud,       UNDETERMINED, REMOVE);
        SET_CONDITIONALLY(extradata, UNDETERMINED, MINIMIZE_2);
        SET_CONDITIONALLY(delete_filler, -1, 3);
        SET_CONDITIONALLY(misc.init,     -1, 7);

        if (ctx->preset == ZDF_PRESET) {
            SET_CONDITIONALLY(ref.init,     -1,  2);
            SET_CONDITIONALLY(qp.init,      -1, 58);
        } else {
            SET_CONDITIONALLY(ref.init,     -1,  1);
            SET_CONDITIONALLY(qp.init,      -1,  0);
        }

        if (ctx->preset == ARD_PRESET || ctx->preset == ZDF_PRESET)
            SET_CONDITIONALLY(reorder_frames, -1, 2);
        else if (ctx->preset == ITUNES_PRESET) {
            SET_CONDITIONALLY(reorder_frames,        -1, 1);
            SET_CONDITIONALLY(fixed_frame_rate_flag, -1, 1);

            if (!ctx->tick_rate.num)
                ctx->tick_rate = (AVRational) { 48000, 1001 };
        }
    }
    #undef SET_CONDITIONALLY

    flags                = ctx->misc.init;
    ctx->misc.misc.flags = flags & (MIN_IDR|MIN_SLICE_TYPE);
    ctx->misc.misc.interlaced =  flags & 4;
    ctx->misc.misc.idr   = 0;

    if (ctx->frame_num_offset)
        ctx->misc.misc.flags |= MOD_FRAME_NUM;
    if (ctx->poc_offset)
        ctx->misc.misc.flags |= MOD_POC;

    if (ctx->extradata == MINIMIZE_2) {
        ctx->header_unused = 1;
        ctx->extradata = MINIMIZE;
    }

    if (ctx->ref.init) {
        int ref = ctx->ref.init;
        ctx->misc.misc.flags |= MOD_REF;
        ref >>= 1;
        ctx->ref.ref.ref[0] = ref & 63;
        if (ctx->ref.ref.ref[0] > 32) {
            return AVERROR(EINVAL);
        }
        ctx->ref.ref.ref[1] = ref >> 6;

        for (i = 1; i >= 0; i--) {
            if (ctx->ref.ref.ref[i] > 0) {
                ctx->ref.ref.ref[i]--;
                ctx->ref.ref.state |= 2 << i;
            }
        }
    }

    if (ctx->qp.init > 1) {
        int8_t *qp_val = ctx->qp.qp.qp_val;
        int     qp     = ctx->qp.init;

        ctx->qp.qp.weighted_pred_flag = qp & 1;
        qp >>= 1;

        for (i = 0;; i++) {
            qp_val[i]  = qp & 63;
            if (qp_val[i] > 52 || (!qp_val[i] && qp))
                return AVERROR(EINVAL);
            qp_val[i] -= 27;
            qp >>= 6;
            if (!qp)
                break;
        }
        ctx->qp.qp.num_qp_minus_1 = i;

        if (i) {
            ctx->qp.qp.state = 1;

            ctx->qp_lut  = av_malloc_array(52, sizeof(*ctx->qp_lut));
            if (!ctx->qp_lut)
                return AVERROR(ENOMEM);
            ctx->qp_lut += 26;
            #define GOL_DIFF(i,j) (i == j ? -1 : av_log2(FFABS(i - j)))
            for (int j = -26; j < 26; j++) {
                int pps_min = 0, len_min = GOL_DIFF(j, qp_val[0]);
                for (int k = 1; k <= i; k++) {
                    int temp = GOL_DIFF(j, qp_val[k]) + (k + 1) / 2;
                    if (temp < len_min) {
                        len_min = temp;
                        pps_min = k;
                    }
                }
                ctx->qp_lut[j] = pps_min;
            }
        } else {
            ctx->misc.misc.flags |= SINGLE_QP;
            ctx->qp.qp.state      = -1;
        }

        if (av_log_get_level() >= AV_LOG_VERBOSE) {
            ctx->qp_histogram = av_mallocz_array(52, sizeof(*ctx->qp_histogram));
            if (!ctx->qp_histogram)
                return AVERROR(ENOMEM);
            ctx->qp_histogram += 26;
        }
    } else {
        ctx->qp.qp.state = ctx->qp.qp.num_qp_minus_1 = 0;
    }

    if (ctx->extradata != PASS && ctx->extradata != REMOVE) {
        err = h264_metadata_alloc_ps(bsf, ctx->extradata == MINIMIZE);
        if (err < 0)
            return err;
    }

    err = ff_cbs_init(&ctx->input,  AV_CODEC_ID_H264, bsf);
    if (err < 0)
        return err;
    err = ff_cbs_init(&ctx->output, AV_CODEC_ID_H264, bsf);
    if (err < 0)
        return err;

    if (bsf->par_in->extradata) {
        CodedBitstreamUnit *pps_unit = NULL;

        err = ff_cbs_read_extradata(ctx->input, au, bsf->par_in);
        if (err < 0) {
            av_log(bsf, AV_LOG_ERROR, "Failed to read extradata.\n");
            goto fail;
        }

        for (i = 0; i < au->nb_units; i++) {
            if (au->units[i].type == H264_NAL_SPS) {
                err = h264_metadata_update_sps(bsf, &au->units[i]);
                if (err < 0)
                    goto fail;
            }

            if (au->units[i].type == H264_NAL_PPS) {
                err = h264_metadata_update_pps(bsf, &au->units[i]);
                if (err < 0)
                    goto fail;
                pps_unit = &au->units[i];
            }
        }

        if (ctx->qp.qp.state > 0) {
            if (!pps_unit) {
                av_log(bsf, AV_LOG_ERROR, "Multiple qp-values need a PPS "
                                          "in extradata.");
                err = AVERROR(EINVAL);
                goto fail;
            } else {
                H264RawPPS *pps;
                for (i = 1; i <= ctx->qp.qp.num_qp_minus_1; i++) {
                    err = ff_cbs_insert_unit_content(au, -1, H264_NAL_PPS,
                                                     pps_unit->content,
                                                     pps_unit->content_ref);
                    if (err < 0)
                        goto fail;

                    pps_unit = &au->units[au->nb_units - 1];

                    err = ff_cbs_make_unit_writable(ctx->output, pps_unit);
                    if (err < 0)
                        goto fail;
                    pps = pps_unit->content;

                    pps->pic_parameter_set_id = i;
                    pps->pic_init_qp_minus26  = ctx->qp.qp.qp_val[i];
                }
            }
        }

        if (ctx->extradata != PASS && ctx->extradata != REMOVE) {
            err = h264_metadata_handle_ps(bsf, au, 2, 1, 0);
            if (err < 0)
                goto fail;

            if (ctx->header) {
                err = h264_metadata_prepare_header(bsf);
                if (err < 0)
                    goto fail;
            }
        }

        err = ff_cbs_write_extradata(ctx->output, bsf->par_out, au);
        if (err < 0) {
            av_log(bsf, AV_LOG_ERROR, "Failed to write extradata.\n");
            goto fail;
        }
    } else if (ctx->qp.qp.state > 0) {
        av_log(bsf, AV_LOG_ERROR, "Multiple qp values need "
                                  "a PPS in extradata.\n");
        return AVERROR_INVALIDDATA;
    } else if (ctx->extradata == REMOVE) {
        av_log(bsf, AV_LOG_WARNING, "Removing in-band extradata would make "
                           "track without out-of-band extradata unplayable."
                           " Switching to passthrough mode for extradata.\n");
        ctx->extradata = PASS;
    } else if (ctx->extradata == MINIMIZE) {
        av_log(bsf, AV_LOG_ERROR, "Minimizing in-band extradata not sensible "
                                  "without out-of-band extradata.\n");
        return AVERROR_INVALIDDATA;
    }

    if (ctx->dts) {
        ctx->dts = -bsf->time_base_out.den * (int64_t)ctx->dts /
                       (bsf->time_base_out.num * 1000LL);
    }

    err = 0;
fail:
    ff_cbs_fragment_reset(au);
    return err;
}

static void h264_metadata_close(AVBSFContext *bsf)
{
    H264MetadataContext *ctx = bsf->priv_data;

    if (ctx->header) {
        Header *header = ctx->header;

        for (int i = 0; i <= header->max_sps; i++)
            av_buffer_unref(&header->sps[i].ps_ref);
        for (int i = 0; i <= header->max_pps; i++)
            av_buffer_unref(&header->pps[i].ps_ref);

        av_freep(&ctx->header);
    }

    if (ctx->extra) {
        Extradata *extra = ctx->extra;

        for (int i = 0; i <= extra->max_sps; i++)
            av_buffer_unref(&extra->sps[i].ps_ref);
        for (int i = 0; i <= extra->max_pps; i++)
            av_buffer_unref(&extra->pps[i].ps_ref);

        av_freep(&ctx->extra);
    }

    ff_cbs_fragment_free(&ctx->access_unit);
    ff_cbs_close(&ctx->input);
    ff_cbs_close(&ctx->output);

    if (ctx->qp_lut) {
        ctx->qp_lut -= 26;
        av_freep(&ctx->qp_lut);
    }

    if (ctx->qp_histogram) {
        ctx->qp_histogram -= 26;
        av_log(bsf, AV_LOG_VERBOSE, "qp-values:");
        for (int i = 0; i < 52; i++)
            av_log(bsf, AV_LOG_VERBOSE, " %i: %i", i, ctx->qp_histogram[i]);
        av_log(bsf, AV_LOG_VERBOSE, "\n");
        av_freep(&ctx->qp_histogram);
    }
}

#define OFFSET(x) offsetof(H264MetadataContext, x)
#define FLAGS (AV_OPT_FLAG_VIDEO_PARAM|AV_OPT_FLAG_BSF_PARAM)
static const AVOption h264_metadata_options[] = {
    { "aud", "Access Unit Delimiter NAL units",
        OFFSET(aud), AV_OPT_TYPE_INT,
        { .i64 = UNDETERMINED }, UNDETERMINED, REMOVE, FLAGS, "aud" },
    { "pass",   NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = PASS   }, .flags = FLAGS, .unit = "aud" },
    { "insert", NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = INSERT }, .flags = FLAGS, .unit = "aud" },
    { "remove", NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = REMOVE }, .flags = FLAGS, .unit = "aud" },

    { "sample_aspect_ratio", "Set sample aspect ratio (table E-1)",
        OFFSET(sample_aspect_ratio), AV_OPT_TYPE_RATIONAL,
        { .dbl = 0.0 }, 0, 65535, FLAGS },

    { "overscan_appropriate_flag", "Set VUI overscan appropriate flag",
        OFFSET(overscan_appropriate_flag), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, 1, FLAGS },

    { "video_format", "Set video format (table E-2)",
        OFFSET(video_format), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, 7, FLAGS},
    { "video_full_range_flag", "Set video full range flag",
        OFFSET(video_full_range_flag), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, 1, FLAGS },
    { "colour_primaries", "Set colour primaries (table E-3)",
        OFFSET(colour_primaries), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, 255, FLAGS },
    { "transfer_characteristics", "Set transfer characteristics (table E-4)",
        OFFSET(transfer_characteristics), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, 255, FLAGS },
    { "matrix_coefficients", "Set matrix coefficients (table E-5)",
        OFFSET(matrix_coefficients), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, 255, FLAGS },

    { "chroma_sample_loc_type", "Set chroma sample location type (figure E-1)",
        OFFSET(chroma_sample_loc_type), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, 6, FLAGS },

    { "tick_rate", "Set VUI tick rate (num_units_in_tick / time_scale)",
        OFFSET(tick_rate), AV_OPT_TYPE_RATIONAL,
        { .dbl = 0.0 }, 0, UINT_MAX, FLAGS },
    { "fixed_frame_rate_flag", "Set VUI fixed frame rate flag",
        OFFSET(fixed_frame_rate_flag), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, 1, FLAGS },
    { "zero_new_constraint_set_flags", "Set constraint_set4_flag / constraint_set5_flag to zero",
        OFFSET(zero_new_constraint_set_flags), AV_OPT_TYPE_BOOL,
        { .i64 = 0 }, 0, 1, FLAGS },

    { "crop_left", "Set left border crop offset",
        OFFSET(crop_left), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, H264_MAX_WIDTH, FLAGS },
    { "crop_right", "Set right border crop offset",
        OFFSET(crop_right), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, H264_MAX_WIDTH, FLAGS },
    { "crop_top", "Set top border crop offset",
        OFFSET(crop_top), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, H264_MAX_HEIGHT, FLAGS },
    { "crop_bottom", "Set bottom border crop offset",
        OFFSET(crop_bottom), AV_OPT_TYPE_INT,
        { .i64 = -1 }, -1, H264_MAX_HEIGHT, FLAGS },

    { "reorder_frames", "Set the number of reorder frames in the VUI",
        OFFSET(reorder_frames), AV_OPT_TYPE_INT,
        {.i64 = -1}, -1, H264_MAX_DPB_FRAMES },

    { "sei_user_data", "Insert SEI user data (UUID+string)",
        OFFSET(sei_user_data), AV_OPT_TYPE_STRING, { .str = NULL }, .flags = FLAGS },

    { "poc_offset", "Offset pic_order_count by the given amount",
        OFFSET(poc_offset), AV_OPT_TYPE_INT, { .i64 = 0}, -65535, 65536 },

    { "frame_num_offset", "Offset frame_num by the given amount",
        OFFSET(frame_num_offset), AV_OPT_TYPE_INT, { .i64 = 0}, -65535, 65536 },

    { "delete_filler", "Delete all filler (both NAL and SEI); if > 1, also delete SEI",
        OFFSET(delete_filler), AV_OPT_TYPE_INT, { .i64 = -1 }, -1, 3, FLAGS},

    { "display_orientation", "Display orientation SEI",
        OFFSET(display_orientation), AV_OPT_TYPE_INT,
        { .i64 = PASS }, PASS, EXTRACT, FLAGS, "disp_or" },
    { "pass",    NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = PASS    }, .flags = FLAGS, .unit = "disp_or" },
    { "insert",  NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = INSERT  }, .flags = FLAGS, .unit = "disp_or" },
    { "remove",  NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = REMOVE  }, .flags = FLAGS, .unit = "disp_or" },
    { "extract", NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = EXTRACT }, .flags = FLAGS, .unit = "disp_or" },

    { "rotate", "Set rotation in display orientation SEI (anticlockwise angle in degrees)",
        OFFSET(rotate), AV_OPT_TYPE_DOUBLE,
        { .dbl = NAN }, -360.0, +360.0, FLAGS },
    { "flip", "Set flip in display orientation SEI",
        OFFSET(flip), AV_OPT_TYPE_FLAGS,
        { .i64 = 0 }, 0, FLIP_HORIZONTAL | FLIP_VERTICAL, FLAGS, "flip" },
    { "horizontal", "Set hor_flip",
        0, AV_OPT_TYPE_CONST,
        { .i64 = FLIP_HORIZONTAL }, .flags = FLAGS, .unit = "flip" },
    { "vertical",   "Set ver_flip",
        0, AV_OPT_TYPE_CONST,
        { .i64 = FLIP_VERTICAL },   .flags = FLAGS, .unit = "flip" },

    { "ref", "num_ref_idx_active_override_flag",
        OFFSET(ref.init), AV_OPT_TYPE_INT, { .i64 = -1 }, -1, 4161, FLAGS},

    { "misc", "Bit 0: Minimize idr_pic_id, bit 1: Minimize slice_type, bit 2: Fix progressive fake-interlaced videos.",
        OFFSET(misc.init), AV_OPT_TYPE_INT, { .i64 = -1 }, -1, 7, FLAGS},

    { "qp", "Modify PPS and slice qp values to create static PPS",
        OFFSET(qp.init), AV_OPT_TYPE_INT, { .i64 = -1 }, -1, 1772526184, FLAGS},

    { "level", "Set level (table A-1)",
        OFFSET(level), AV_OPT_TYPE_INT,
        { .i64 = LEVEL_UNSET }, LEVEL_UNSET, 0xff, FLAGS, "level" },
    { "auto", "Attempt to guess level from stream properties",
        0, AV_OPT_TYPE_CONST,
        { .i64 = LEVEL_AUTO }, .flags = FLAGS, .unit = "level" },
#define LEVEL(name, value) name, NULL, 0, AV_OPT_TYPE_CONST, \
        { .i64 = value },      .flags = FLAGS, .unit = "level"
    { LEVEL("1",   10) },
    { LEVEL("1b",   9) },
    { LEVEL("1.1", 11) },
    { LEVEL("1.2", 12) },
    { LEVEL("1.3", 13) },
    { LEVEL("2",   20) },
    { LEVEL("2.1", 21) },
    { LEVEL("2.2", 22) },
    { LEVEL("3",   30) },
    { LEVEL("3.1", 31) },
    { LEVEL("3.2", 32) },
    { LEVEL("4",   40) },
    { LEVEL("4.1", 41) },
    { LEVEL("4.2", 42) },
    { LEVEL("5",   50) },
    { LEVEL("5.1", 51) },
    { LEVEL("5.2", 52) },
    { LEVEL("6",   60) },
    { LEVEL("6.1", 61) },
    { LEVEL("6.2", 62) },
#undef LEVEL

    { "dts", "Offset dts in ms",
        OFFSET(dts), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, INT_MAX, FLAGS },

    { "preset", "Set some values with lower priority than a given argument.",
        OFFSET(preset), AV_OPT_TYPE_INT, { .i64 = DEFAULT_PRESET },
        DEFAULT_PRESET, ITUNES_PRESET, FLAGS, "preset" },
    { "default", NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = DEFAULT_PRESET     }, .flags = FLAGS, .unit = "preset" },
    { "pass",    NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = PASSTHROUGH_PRESET }, .flags = FLAGS, .unit = "preset" },
    { "ARD",     NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = ARD_PRESET         }, .flags = FLAGS, .unit = "preset" },
    { "ZDF",     NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = ZDF_PRESET         }, .flags = FLAGS, .unit = "preset" },
    { "itunes",     NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = ITUNES_PRESET      }, .flags = FLAGS, .unit = "preset" },

    { "extradata", "How to treat extradata.",
        OFFSET(extradata), AV_OPT_TYPE_INT, { .i64 = UNDETERMINED },
        MINIMIZE, REMOVE, FLAGS, "extra" },
    { "minimize", NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = MINIMIZE   }, .flags = FLAGS, .unit = "extra" },
    { "minimize_2", NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = MINIMIZE_2 }, .flags = FLAGS, .unit = "extra" },
    { "insert_2", NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = INSERT_2   }, .flags = FLAGS, .unit = "extra" },
    { "insert_3", NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = INSERT_3   }, .flags = FLAGS, .unit = "extra" },
    { "pass",     NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = PASS       }, .flags = FLAGS, .unit = "extra" },
    { "insert",   NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = INSERT     }, .flags = FLAGS, .unit = "extra" },
    { "remove",   NULL, 0, AV_OPT_TYPE_CONST,
        { .i64 = REMOVE     }, .flags = FLAGS, .unit = "extra" },

    { NULL }
};

static const AVClass h264_metadata_class = {
    .class_name = "h264_metadata_bsf",
    .item_name  = av_default_item_name,
    .option     = h264_metadata_options,
    .version    = LIBAVUTIL_VERSION_INT,
};

static const enum AVCodecID h264_metadata_codec_ids[] = {
    AV_CODEC_ID_H264, AV_CODEC_ID_NONE,
};

const AVBitStreamFilter ff_h264_metadata_bsf = {
    .name           = "h264_metadata",
    .priv_data_size = sizeof(H264MetadataContext),
    .priv_class     = &h264_metadata_class,
    .init           = &h264_metadata_init,
    .close          = &h264_metadata_close,
    .filter         = &h264_metadata_filter,
    .codec_ids      = h264_metadata_codec_ids,
};
