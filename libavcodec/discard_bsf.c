#include <stdlib.h>
#include "libavutil/avassert.h"
#include "bsf.h"
#include "bsf_internal.h"

#define MAX_REORDER_PACKETS 32  /* H.264 DBP size is 16 frames = 32 fields */

typedef struct DiscardContext {
    enum { WAITING_FOR_KEYFRAME, SEEN_KEYFRAME, FLUSHING, DONE } status;
    int num_pts;
    int64_t keyframe_pts;
    unsigned first_packet, last_packet; /* packets[first_packet..(last_packet - 1)] are valid */
    AVPacket packets[MAX_REORDER_PACKETS + 1]; /* + 1 in order to be able to store the packet with pts > keyframe_pts even when MAX_REORDER_PACKETS of ref frames preceded the keyframe according to PTS */
    int64_t pts[MAX_REORDER_PACKETS + 1];
} DiscardContext;

typedef struct PTS {
    int64_t pts;
    int idx;
} PTS;

static int cmp_pts(const void *x, const void *y)
{
    const PTS *a = x, *b = y;
    if (a->pts != b->pts)
        return (a->pts < b->pts) - (a->pts > b->pts);
    /* When two PTS are equal, give the first packet in coding order
     * the lower PTS. */
    return (a->idx < b->idx) - (a->idx > b->idx);
}

static void prepare_flushing(DiscardContext *ctx)
{
    PTS pts[MAX_REORDER_PACKETS + 1];

    for (int i = 0; i < ctx->last_packet; i++) {
        pts[i].pts = ctx->packets[i].pts;
        pts[i].idx = i;
    }
    qsort(pts, ctx->last_packet, sizeof(*pts), cmp_pts);
    /* pts[i].idx == j means now that the ith packet has the jth biggest pts,
     * i.e. it should receive the pts ctx->pts[j]. */
    for (int i = 0; i < ctx->last_packet; i++) {
        int j = pts[i].idx;
        av_assert0(ctx->packets[i].pts <= ctx->pts[j]);
        ctx->packets[i].pts = ctx->pts[j];
    }
    ctx->status = FLUSHING;
}

static int discard_filter(AVBSFContext *bsf, AVPacket *out)
{
    DiscardContext *const ctx = bsf->priv_data;
    int ret;

    if (ctx->status == WAITING_FOR_KEYFRAME || ctx->status == DONE) {
        av_assert0(ctx->first_packet == ctx->last_packet);
        ret = ff_bsf_get_packet_ref(bsf, out);
        if (ret < 0)
            return ret;
        if (ctx->status == DONE)
            return 0;
        if (out->flags & AV_PKT_FLAG_KEY) {
            if (out->pts != AV_NOPTS_VALUE) {
                ctx->keyframe_pts = out->pts;
                ctx->status       = SEEN_KEYFRAME;
            } else {
                ctx->status       = DONE;
            }
        } else if (out->flags & AV_PKT_FLAG_DISPOSABLE) {
            av_packet_unref(out);
            return AVERROR(EAGAIN);
        }
        return 0;
    }
    if (ctx->status == SEEN_KEYFRAME) {
        AVPacket *const pkt = &ctx->packets[ctx->last_packet];
        av_assert0(ctx->last_packet < FF_ARRAY_ELEMS(ctx->packets));
        av_assert0(ctx->first_packet == 0);
        av_assert0(ctx->num_pts >= ctx->last_packet);
        ret = ff_bsf_get_packet_ref(bsf, pkt);
        if (ret == AVERROR_EOF && ctx->last_packet > 0) {
            prepare_flushing(ctx);
        } else if (ret < 0) {
            return ret;
        } else {
            int i;

            for (i = 0; i < ctx->num_pts; i++)
                if (pkt->pts > ctx->pts[i])
                    break;

            if (i < FF_ARRAY_ELEMS(ctx->pts)) {
                memmove(&ctx->pts[i + 1], &ctx->pts[i],
                        FFMIN(FF_ARRAY_ELEMS(ctx->pts) - i + 1, ctx->num_pts - i) * sizeof(ctx->pts[0]));
                ctx->pts[i]  = pkt->pts;
                ctx->num_pts = FFMIN(ctx->num_pts + 1, FF_ARRAY_ELEMS(ctx->pts));
            }

            if (((pkt->flags & (AV_PKT_FLAG_KEY | AV_PKT_FLAG_DISPOSABLE))
                 == AV_PKT_FLAG_DISPOSABLE) && pkt->pts < ctx->keyframe_pts) {
                av_packet_unref(pkt);
                return AVERROR(EAGAIN);
            }
            ctx->last_packet++;
            if (pkt->pts >= ctx->keyframe_pts) {
                prepare_flushing(ctx);
            } else if (ctx->last_packet == FF_ARRAY_ELEMS(ctx->packets)) {
                av_log(bsf, AV_LOG_ERROR, "Encountered too many leading "
                       "reference frames. Flushing.\n");
                prepare_flushing(ctx);
            } else
                return AVERROR(EAGAIN);
        }
    }
    av_assert0(ctx->status == FLUSHING);
    av_assert0(ctx->first_packet < ctx->last_packet);
    av_packet_move_ref(out, &ctx->packets[ctx->first_packet++]);
    if (ctx->first_packet == ctx->last_packet)
        ctx->status = DONE;

    return 0;
}

static av_cold void discard_close(AVBSFContext *bsf)
{
    DiscardContext *const ctx = bsf->priv_data;

    for (int i = 0; i < FF_ARRAY_ELEMS(ctx->packets); i++)
        av_packet_unref(&ctx->packets[i]);
}

const AVBitStreamFilter ff_discard_bsf = {
    .name           = "discard",
    .priv_data_size = sizeof(DiscardContext),
    .filter         = discard_filter,
    .close          = discard_close,
};
