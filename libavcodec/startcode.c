/*
 * Copyright (c) 2003-2010 Michael Niedermayer <michaelni@gmx.at>
 *
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

/**
 * @file
 * Accelerated start code search function for start codes common to
 * MPEG-1/2/4 video, VC-1, H.264/5
 * @author Michael Niedermayer <michaelni@gmx.at>
 */

#include "startcode.h"
#include "config.h"
#include "libavutil/intreadwrite.h"

int ff_startcode_find_candidate_c(const uint8_t *buf, int size)
{
    const uint8_t *start = buf, *end = buf + size;

#if HAVE_FAST_UNALIGNED
#define READ(bitness) AV_RN ## bitness
#define MAIN_LOOP(bitness, mask1, mask2) do {                              \
        /* we check p < end instead of p + 3 / 7 because it is
         * simpler and there must be AV_INPUT_BUFFER_PADDING_SIZE
         * bytes at the end. */                                            \
        for (; buf < end; buf += bitness / 8)                              \
            if ((~READ(bitness)(buf) & (READ(bitness)(buf) - mask1))       \
                                     & mask2)                              \
                break;                                                     \
    } while (0)

#if HAVE_FAST_64BIT
    MAIN_LOOP(64, 0x0101010101010101ULL, 0x8080808080808080ULL);
#else
    MAIN_LOOP(32, 0x01010101U, 0x80808080U);
#endif
#endif
    for (; buf < end; buf++)
        if (!*buf)
            break;
    return buf - start;
}
