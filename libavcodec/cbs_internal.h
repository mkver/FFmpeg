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

#ifndef AVCODEC_CBS_INTERNAL_H
#define AVCODEC_CBS_INTERNAL_H

#include "avcodec.h"
#include "cbs.h"
#include "get_bits.h"
#include "put_bits.h"


typedef struct CodedBitstreamType {
    enum AVCodecID codec_id;

    size_t priv_data_size;

    // Split frag->data into coded bitstream units, creating the
    // frag->units array.  Fill data but not content on each unit.
    // The header argument should be set if the fragment came from
    // a header block, which may require different parsing for some
    // codecs (e.g. the AVCC header in H.264).
    int (*split_fragment)(CodedBitstreamContext *ctx,
                          CodedBitstreamFragment *frag,
                          int header);

    // Read the unit->data bitstream and decompose it, creating
    // unit->content.
    int (*read_unit)(CodedBitstreamContext *ctx,
                     CodedBitstreamUnit *unit);

    // Make a unit's content writable.
    int (*make_writable)(CodedBitstreamContext *ctx,
                         CodedBitstreamUnit *unit);

    // Write the data bitstream from unit->content into pbc.
    // Return value AVERROR(ENOSPC) indicates that pbc was too small.
    int (*write_unit)(CodedBitstreamContext *ctx,
                      CodedBitstreamUnit *unit,
                      PutBitContext *pbc);

    // Read the data from all of frag->units and assemble it into
    // a bitstream for the whole fragment.
    int (*assemble_fragment)(CodedBitstreamContext *ctx,
                             CodedBitstreamFragment *frag);

    // Free the codec internal state.
    void (*close)(CodedBitstreamContext *ctx);
} CodedBitstreamType;


// Helper functions for trace output.

void ff_cbs_trace_header(CodedBitstreamContext *ctx,
                         const char *name);

void ff_cbs_trace_syntax_element(CodedBitstreamContext *ctx, int position,
                                 const char *name, const int *subscripts,
                                 const char *bitstring, int64_t value);


// Helper functions for read/write of common bitstream elements, including
// generation of trace output.

int ff_cbs_read_unsigned(CodedBitstreamContext *ctx, GetBitContext *gbc,
                         int width, const char *name,
                         const int *subscripts, uint32_t *write_to,
                         uint32_t range_min, uint32_t range_max);

int ff_cbs_write_unsigned(CodedBitstreamContext *ctx, PutBitContext *pbc,
                          int width, const char *name,
                          const int *subscripts, uint32_t value,
                          uint32_t range_min, uint32_t range_max);

int ff_cbs_read_signed(CodedBitstreamContext *ctx, GetBitContext *gbc,
                       int width, const char *name,
                       const int *subscripts, int32_t *write_to,
                       int32_t range_min, int32_t range_max);

int ff_cbs_write_signed(CodedBitstreamContext *ctx, PutBitContext *pbc,
                        int width, const char *name,
                        const int *subscripts, int32_t value,
                        int32_t range_min, int32_t range_max);

// The largest unsigned value representable in N bits, suitable for use as
// range_max in the above functions.
#define MAX_UINT_BITS(length) ((UINT64_C(1) << (length)) - 1)

// The largest signed value representable in N bits, suitable for use as
// range_max in the above functions.
#define MAX_INT_BITS(length) ((INT64_C(1) << ((length) - 1)) - 1)

// The smallest signed value representable in N bits, suitable for use as
// range_min in the above functions.
#define MIN_INT_BITS(length) (-(INT64_C(1) << ((length) - 1)))


extern const CodedBitstreamType ff_cbs_type_av1;
extern const CodedBitstreamType ff_cbs_type_h264;
extern const CodedBitstreamType ff_cbs_type_h265;
extern const CodedBitstreamType ff_cbs_type_jpeg;
extern const CodedBitstreamType ff_cbs_type_mpeg2;
extern const CodedBitstreamType ff_cbs_type_vp9;


enum {
    BITS,
    BYTES
};

// The following macro automatically creates both (deep) copy and
// free functions for structs with exactly one internal buffer.

#define cbs_copy_free(codec, type, var, buffer, size_element, size_offset, size_unit) \
static void cbs_ ## codec ## _free_ ## var(void *unit, uint8_t *content) \
{ \
    type *var = (type *)content; \
 \
    av_buffer_unref(&var->buffer ## _ref); \
    av_freep(&var); \
} \
 \
static AVBufferRef *cbs_ ## codec ## _copy_ ## var(const type *source) \
{ \
    AVBufferRef *copy_ref; \
    type *copy; \
 \
    copy = av_malloc(sizeof(type)); \
    if (!copy) \
        return NULL; \
    memcpy(copy, source, sizeof(type)); \
 \
    copy_ref = av_buffer_create((uint8_t*)copy, sizeof(type), \
                            &cbs_ ## codec ## _free_ ## var, \
                            NULL, 0); \
    if (!copy_ref) { \
        av_free(copy); \
        return NULL; \
    } \
 \
    if (source->buffer) { \
        size_t size = (size_t)source->size_element + size_offset; \
        if (size_unit == BITS) \
            size = (size + 7) / 8; \
 \
        copy->buffer ## _ref = av_buffer_alloc(size + AV_INPUT_BUFFER_PADDING_SIZE); \
        if (!copy->buffer ## _ref) {\
            av_buffer_unref(&copy_ref); \
            return NULL; \
        } \
        copy->buffer = copy->buffer ## _ref->data; \
        memcpy(copy->buffer, source->buffer, size); \
        memset(copy->buffer + size, 0, AV_INPUT_BUFFER_PADDING_SIZE); \
    } \
 \
    return copy_ref; \
}

#endif /* AVCODEC_CBS_INTERNAL_H */
