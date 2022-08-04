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

typedef struct AVRefCount {
    atomic_uintptr_t refcount;
    void *opaque;
    void (*free)(void *opaque, void *data);
} AVRefCount;

#if __STDC_VERSION__ >= 201112L
#define REFCOUNT_OFFSET FFALIGN(sizeof(AVRefCount), FFMAX3(STRIDE_ALIGN, 16, _Alignof(max_align_t)))
#else
#define REFCOUNT_OFFSET FFALIGN(sizeof(AVRefCount), FFMAX(STRIDE_ALIGN, 16))
#endif

static void refcount_init(AVRefCount *ref, void *opaque, void (*free)(void *opaque, void *data))
{
    atomic_init(&ref->refcount, 1);
    ref->opaque = opaque;
    ref->free   = free;
}

void *ff_refstruct_allocz_ext(size_t size, void *opaque,
                             void (*free)(void *opaque, void *data))
{
    void *buf, *data;

    if (size > SIZE_MAX - REFCOUNT_OFFSET)
        return NULL;
    buf = av_malloc(size + REFCOUNT_OFFSET);
    if (!buf)
        return NULL;
    refcount_init(buf, opaque, free);
    data = (char*)buf + REFCOUNT_OFFSET;
    memset(data, 0, size);

    return data;
}

void *ff_refstruct_allocz(size_t size)
{
    return ff_refstruct_allocz_ext(size, NULL, NULL);
}

void ff_refstruct_unref(void *datap)
{
    char *data = *(void**)datap;
    AVRefCount *ref;

    if (!data)
        return;
    *(void**)datap = NULL;

    ref = (AVRefCount*)(data - REFCOUNT_OFFSET);
    if (atomic_fetch_sub_explicit(&ref->refcount, 1, memory_order_acq_rel) == 1) {
        if (ref->free)
            ref->free(ref->opaque, data);
        av_free(ref);
    }

    return;
}

void *ff_refstruct_ref(const void *data)
{
    AVRefCount *ref = (AVRefCount*)((char*)data - REFCOUNT_OFFSET);

    atomic_fetch_add_explicit(&ref->refcount, 1, memory_order_relaxed);

    return (void*)data;
}

void ff_refstruct_replace(void *dstp, const void *src)
{
    void *dst = *(void**)dstp;

    if (src == dst)
        return;
    ff_refstruct_unref(dstp);
    if (src)
        *(void**)dstp = ff_refstruct_ref(src);
}

int ff_refstruct_is_writable(const void *buf)
{
    AVRefCount *ref = (AVRefCount*)((char *)buf - REFCOUNT_OFFSET);
    return atomic_load_explicit(&ref->refcount, memory_order_acquire) == 1;
}
