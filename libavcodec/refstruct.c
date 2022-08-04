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

#include <stdatomic.h>

#include "internal.h"
#include "refstruct.h"

typedef struct RefCount {
    atomic_uintptr_t refcount;
    void *opaque;
    void (*free)(void *opaque, void *data);
} RefCount;

#if __STDC_VERSION__ >= 201112L
#define REFCOUNT_OFFSET FFALIGN(sizeof(RefCount), FFMAX3(STRIDE_ALIGN, 16, _Alignof(max_align_t)))
#else
#define REFCOUNT_OFFSET FFALIGN(sizeof(RefCount), FFMAX(STRIDE_ALIGN, 16))
#endif

static RefCount *get_refcount(void *data)
{
    return (RefCount*)((char*)data - REFCOUNT_OFFSET);
}

static void *get_userdata(void *buf)
{
    return (char*)buf + REFCOUNT_OFFSET;
}

static void refcount_init(RefCount *ref, void *opaque, void (*free)(void *opaque, void *data))
{
    atomic_init(&ref->refcount, 1);
    ref->opaque = opaque;
    ref->free   = free;
}

void *ff_refstruct_alloc_ext(size_t size, unsigned flags, void *opaque,
                             void (*free)(void *opaque, void *data))
{
    void *buf, *data;

    if (size > SIZE_MAX - REFCOUNT_OFFSET)
        return NULL;
    buf = av_malloc(size + REFCOUNT_OFFSET);
    if (!buf)
        return NULL;
    refcount_init(buf, opaque, free);
    data = get_userdata(buf);
    if (!(flags & FF_REFSTRUCT_FLAG_NO_ZEROING))
        memset(data, 0, size);

    return data;
}

void *ff_refstruct_allocz(size_t size)
{
    return ff_refstruct_alloc_ext(size, 0, NULL, NULL);
}

void ff_refstruct_unref(void *datap)
{
    void *data;
    RefCount *ref;

    memcpy(&data, datap, sizeof(data));
    if (!data)
        return;
    memcpy(datap, &(void *){ NULL }, sizeof(data));

    ref = get_refcount(data);
    if (atomic_fetch_sub_explicit(&ref->refcount, 1, memory_order_acq_rel) == 1) {
        if (ref->free)
            ref->free(ref->opaque, data);
        av_free(ref);
    }

    return;
}

void *ff_refstruct_ref(void *data)
{
    RefCount *ref = get_refcount(data);

    atomic_fetch_add_explicit(&ref->refcount, 1, memory_order_relaxed);

    return data;
}

const void *ff_refstruct_ref_c(const void *data)
{
    /* Casting const away here is fine, as it is only supposed
     * to apply to the user's data and not our bookkeeping data. */
    RefCount *ref = get_refcount((void*)data);

    atomic_fetch_add_explicit(&ref->refcount, 1, memory_order_relaxed);

    return data;
}

void ff_refstruct_replace(void *dstp, const void *src)
{
    const void *dst;
    memcpy(&dst, dstp, sizeof(dst));

    if (src == dst)
        return;
    ff_refstruct_unref(dstp);
    if (src) {
        dst = ff_refstruct_ref_c(src);
        memcpy(dstp, &dst, sizeof(dst));
    }
}
