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

/* Typedef that may be used for pointers that are refcounted
 * via this API. Using this typedef is not mandatory;
 * it is useful in case a pointer is only retained
 * due to its refcount and not to access its data. */
typedef void* FFRefStruct;

void *ff_refstruct_allocz_ext(size_t size, void *opaque,
                              void (*free)(void *opaque, void *buf));

void *ff_refstruct_allocz(size_t size);

void ff_refstruct_unref(void *bufp);

void *ff_refstruct_ref(const void *buf);

void ff_refstruct_replace(void *dstp, const void *src);

int ff_refstruct_is_writable(const void *buf);

#endif /* AVCODEC_REFSTRUCT_H */
