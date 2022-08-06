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

#include "libavutil/avassert.h"
#include "libavutil/thread.h"
#include "internal.h"
#include "refstruct.h"

typedef struct RefCount {
    atomic_uintptr_t refcount;
    void *opaque;
    void (*free)(void *opaque, void *data);
    int no_auto_free; ///< used by the FFRefStructPool to reuse the buffers
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

static const RefCount *cget_refcount(const void *data)
{
    return (const RefCount*)((const char*)data - REFCOUNT_OFFSET);
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
    ref->no_auto_free = 0;
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
        /* ref->free below might already free ref in case the pool API is in use,
         * so we have to read the flag now to avoid use-after-free. */
        int to_free = !ref->no_auto_free;
        if (ref->free)
            ref->free(ref->opaque, data);
        if (to_free)
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

int ff_refstruct_is_writable(const void *data)
{
    const RefCount *ref = cget_refcount(data);
    /* Casting const away here is safe, because it is a load.
     * It is necessary because atomic_load_explicit() does not
     * accept const atomics in C11 (see also N1807). */
    return atomic_load_explicit((atomic_uintptr_t*)&ref->refcount, memory_order_acquire) == 1;
}

struct FFRefStructPool {
    size_t size;
    void *opaque;
    int (*init)(void *opaque, void *buf);
    void (*reset)(void *opaque, void *buf);
    void (*free_entry)(void *opaque, void *buf);
    void (*free)(void *opaque);

    unsigned pool_flags;
    unsigned entry_flags;

    int uninited;
    /* The number of outstanding entries not in available_entries. */
    atomic_uintptr_t refcount;
    /* This is a linked list of available entries;
     * the AVRefCount's opaque pointer is used as next pointer
     * for available entries.
     * While the entries are in use, the opaque is a pointer
     * to the corresponding FFRefStructPool. */
    void *available_entries;
    pthread_mutex_t mutex;
};

static void pool_free(FFRefStructPool *pool)
{
    pthread_mutex_destroy(&pool->mutex);
    if (pool->free)
        pool->free(pool->opaque);
    av_free(pool);
}

static void pool_free_entry(FFRefStructPool *pool, void *entry)
{
    if (pool->free_entry)
        pool->free_entry(pool->opaque, entry);
    av_free(get_refcount(entry));
}

static void pool_release_entry(void *opaque, void *entry)
{
    FFRefStructPool *pool = opaque;
    RefCount *ref = get_refcount(entry);

    if (pool->reset)
        pool->reset(pool->opaque, entry);

    pthread_mutex_lock(&pool->mutex);
    if (!pool->uninited) {
        ref->opaque = pool->available_entries;
        pool->available_entries = entry;
        entry = NULL;
    }
    pthread_mutex_unlock(&pool->mutex);

    if (entry)
        pool_free_entry(pool, entry);

    if (atomic_fetch_sub_explicit(&pool->refcount, 1, memory_order_acq_rel) == 1)
        pool_free(pool);
}

void *ff_refstruct_pool_get(FFRefStructPool *pool)
{
    void *ret = NULL;

    pthread_mutex_lock(&pool->mutex);
    av_assert1(!pool->uninited);
    if (pool->available_entries) {
        RefCount *ref = get_refcount(pool->available_entries);
        ret = pool->available_entries;
        pool->available_entries = ref->opaque;
        ref->opaque = pool;
        atomic_init(&ref->refcount, 1);
    }
    pthread_mutex_unlock(&pool->mutex);

    if (!ret) {
        RefCount *ref;
        ret = ff_refstruct_alloc_ext(pool->size, pool->entry_flags,
                                     pool, pool_release_entry);
        if (!ret)
            return NULL;
        ref = get_refcount(ret);
        ref->no_auto_free = 1;
        if (pool->init) {
            int err = pool->init(pool->opaque, ret);
            if (err < 0) {
                //
                av_free(ref);
                return NULL;
            }
        }
    }
    atomic_fetch_add_explicit(&pool->refcount, 1, memory_order_relaxed);
    if (pool->pool_flags & FF_REFSTRUCT_POOL_FLAG_ZERO_EVERY_TIME)
        memset(ret, 0, pool->size);
    return ret;
}

void ff_refstruct_pool_uninit(FFRefStructPool **poolp)
{
    FFRefStructPool *pool = *poolp;

    if (!pool)
        return;

    pthread_mutex_lock(&pool->mutex);
    av_assert1(!pool->uninited);
    pool->uninited = 1;
    pthread_mutex_unlock(&pool->mutex);

    /* Because we set uninited above, none of the still outstanding
     * entries will be added to available_entries, so that no one
     * except us touches available entries. Therefore we are allowed
     * to access available_entries without holding the lock. */
    for (void *entry = pool->available_entries; entry; ) {
        void *next = get_refcount(entry)->opaque;
        pool_free_entry(pool, entry);
        entry = next;
    }
    pool->available_entries = NULL;

    if (atomic_fetch_sub_explicit(&pool->refcount, 1, memory_order_acq_rel) == 1)
        pool_free(pool);

    *poolp = NULL;
}

FFRefStructPool *ff_refstruct_pool_alloc(size_t size, unsigned flags)
{
    return ff_refstruct_pool_alloc_ext(size, flags, NULL, NULL, NULL, NULL, NULL);
}

FFRefStructPool *ff_refstruct_pool_alloc_ext(size_t size, unsigned flags,
                                             void *opaque,
                                             int (*init)(void *opaque, void *buf),
                                             void (*reset)(void *opaque, void *buf),
                                             void (*free_entry)(void *opaque, void *buf),
                                             void (*free)(void *opaque))
{
    FFRefStructPool *pool = av_mallocz(sizeof(*pool));
    int err;

    if (!pool)
        return NULL;

    pool->size   = size;
    pool->opaque = opaque;
    pool->init   = init;
    pool->reset  = reset;
    pool->free_entry = free_entry;
    pool->free   = free;
    pool->pool_flags = flags;
#define ALL_ENTRY_FLAGS FF_REFSTRUCT_FLAG_NO_ZEROING
    pool->entry_flags = flags & ALL_ENTRY_FLAGS;
    if (flags & FF_REFSTRUCT_POOL_FLAG_ZERO_EVERY_TIME) {
        // We will zero the buffer before every use, so zeroing
        // upon allocating the buffer is unnecessary.
        pool->entry_flags |= FF_REFSTRUCT_FLAG_NO_ZEROING;
    }

    atomic_init(&pool->refcount, 1);

    err = pthread_mutex_init(&pool->mutex, NULL);
    if (err) {
        av_free(pool);
        return NULL;
    }
    return pool;
}
