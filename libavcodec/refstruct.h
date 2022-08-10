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

#ifndef AVCODEC_REFSTRUCT_H
#define AVCODEC_REFSTRUCT_H

#include <stddef.h>

typedef struct FFRefStructPool FFRefStructPool;

typedef union FFRefStructFreeCB {
    void (*free)(void *opaque, void *data);
    void (*free_ext)(void *opaque, void *initial_opaque, void *data);
} FFRefStructFreeCB;

#define FF_REFSTRUCT_FLAG_NO_ZEROING (1 << 0)
/* This flag being set indicates that the free_cb union is in the free_ext-state.
 * In this case unreferencing the data has to be done
 * via ff_refstruct_unref_ext() instead of ff_refstruct_unref().
 * Using the latter is forbidden.
 * The free_ext-callback will be called when the refcount reaches zero
 * with the opaque given as argument to ff_refstruct_unref_ext() being
 * the first opaque argument given to the callback; the argument corresponding
 * for initial opaque will be the opaque given to ff_refstruct_alloc_ext()
 * and data is the entry to be freed. */
#define FF_REFSTRUCT_FLAG_DYNAMIC_OPAQUE   (1 << 1)

void *ff_refstruct_alloc_ext2(size_t size, unsigned flags, void *opaque,
                              FFRefStructFreeCB free_cb);

static inline
void *ff_refstruct_alloc_ext(size_t size, unsigned flags, void *opaque,
                             void (*free)(void *opaque, void *data))
{
    return ff_refstruct_alloc_ext2(size, flags, opaque, (FFRefStructFreeCB) { .free = free });
}

void *ff_refstruct_allocz(size_t size);

void ff_refstruct_unref_ext(void *opaque, void *datap);
void ff_refstruct_unref(void *datap);

const void *ff_refstruct_ref_c(const void *data);

void *ff_refstruct_ref(void *data);

void ff_refstruct_replace(void *dstp, const void *src);

int ff_refstruct_is_writable(const void *data);

typedef union FFRefStructPoolResetCB {
    void (*reset)(void *opaque, void *data);
    void (*reset_ext)(void *opaque, void *pool_opaque, void *data);
} FFRefStructPoolResetCB;

#define FF_REFSTRUCT_POOL_FLAG_ZERO_EVERY_TIME  (1 << 16)
/* This flag being set indicates that the reset_cb union is in the reset_ext-state.
 * In this case unreferencing the pool entries has to be done
 * via ff_refstruct_unref_ext() instead of ff_refstruct_unref().
 * Using the latter is forbidden.
 * The reset_ext-callback will be called when the refcount reaches zero
 * with the opaque given as argument to ff_refstruct_unref_ext() being
 * the first opaque argument given to the callback; the argument corresponding
 * to pool_opaque will be the opaque given to ff_refstruct_pool_alloc_ext()
 * and data is the entry to be reset. */
#define FF_REFSTRUCT_POOL_FLAG_DYNAMIC_OPAQUE   (1 << 17)

FFRefStructPool *ff_refstruct_pool_alloc(size_t size, unsigned flags);

FFRefStructPool *ff_refstruct_pool_alloc_ext2(size_t size, unsigned flags,
                                              void *opaque,
                                              int (*init)(void *opaque, void *buf),
                                              FFRefStructPoolResetCB reset_cb,
                                              void (*free_entry)(void *opaque, void *buf),
                                              void (*free)(void *opaque));

static inline
FFRefStructPool *ff_refstruct_pool_alloc_ext(size_t size, unsigned flags,
                                             void *opaque,
                                             int (*init)(void *opaque, void *buf),
                                             void (*reset)(void *opaque, void *data),
                                             void (*free_entry)(void *opaque, void *buf),
                                             void (*free)(void *opaque))
{
    return ff_refstruct_pool_alloc_ext2(size, flags, opaque, init,
                                        (FFRefStructPoolResetCB){.reset = reset },
                                        free_entry, free);
}

void *ff_refstruct_pool_get(FFRefStructPool *pool);

void ff_refstruct_pool_uninit(FFRefStructPool **pool);

#endif /* AVCODEC_REFSTRUCT_H */
