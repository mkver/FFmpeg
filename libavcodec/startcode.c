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
 * @author Michael Niedermayer <michaelni@gmx.at>,
 * @author Andreas Rheinhardt <andreas.rheinhardt@gmail.com>
 */

#include "startcode.h"
#include "config.h"
#include "libavutil/intreadwrite.h"
#include "libavutil/common.h"


#if AV_GCC_VERSION_AT_LEAST(2,96) || defined(__clang__)
# define av_likely(x)      __builtin_expect(!!(x), 1)
# define av_unlikely(x)    __builtin_expect(!!(x), 0)
#else
# define av_likely(x)      (x)
# define av_unlikely(x)    (x)
#endif


int ff_startcode_find_candidate_c(const uint8_t *buf, int size)
{
    int i = 1, temp;

#define INITIALIZATION(mod) do {                                           \
        if (size <= mod + 1)                                               \
            goto near_end;                                                 \
        /* From this point on until the end of the MAIN_LOOP,              \
         * buf is the earliest possible position of a 0x00                 \
         * immediately preceding a startcode's 0x01, i.e.                  \
         * everything from start to buf (inclusive) is known               \
         * to not contain a startcode's 0x01. */                           \
        temp = (const uint8_t *)((uintptr_t)(buf + i + mod - 1) / mod * mod) - buf;  \
        size-= mod;                                                        \
        goto startcode_check;                                              \
    } while (0)

#define READ(bitness) AV_RN ## bitness ## A
#define STARTCODE_CHECK do { \
        if (buf[i + 1] > 1) {                                              \
            i += 3;                                                        \
        } else if (buf[i]) {                                               \
            i += 2;                                                        \
        } else if (buf[i - 1] || buf[i + 1] == 0) {                        \
            i += 1;                                                        \
        } else {                                                           \
            goto found_startcode;                                          \
        }                                                                  \
    } while (0)

    /* The MAIN_LOOP tries to read several bytes at once.
     * A startcode's 0x00 0x01 or 0x00 0x00 will be detected
     * by it if these bytes are contained within the bytes
     * read at once. */
#define MAIN_LOOP(bitness, mask1, mask2) do {                              \
        for (; i < size ; ) {                                              \
            if (av_likely(!((~READ(bitness)(buf + i) & (READ(bitness)(buf + i) - mask1))\
                                           & mask2))) {                    \
                i += bitness / 8;                                          \
                continue;                                                  \
            }                                                              \
            temp = i + bitness / 8;                                        \
            /* If the 0x01 of a startcode is at position buf + i + 1,      \
             * the following check detects it.                             \
             * Because buf got incremented before entering this            \
             * loop, buf - 1 may be evaluated.                             \
             * Because temp + 1 < start + size, buf is always              \
             * <= the end of the buffer during the closing check           \
             * so that the pointer comparison is defined.                  \
             * Because i gets initialized to 1,                            \
             * buf + i - 1 may be evaluated. */                            \
        startcode_check:                                                   \
            do {                                                           \
                STARTCODE_CHECK;                                           \
            } while (i < temp);                                            \
            i = temp;                                                      \
        }                                                                  \
        /* Revert to the real size. */                                     \
        size += bitness / 8;                                               \
    } while (0)

#if HAVE_FAST_64BIT
    INITIALIZATION(8);
#if HAVE_BIGENDIAN
    MAIN_LOOP(64, 0x0002000200020002ULL, 0x8000800080008000ULL);
#else
    MAIN_LOOP(64, 0x0010010100100101ULL, 0x8008008080080080ULL);
#endif
#else
    INITIALIZATION(4);
#if HAVE_BIGENDIAN
    MAIN_LOOP(32, 0x00020002U, 0x80008000U);
#else
    MAIN_LOOP(32, 0x00100101U, 0x80080080U);
#endif
#endif

near_end:
    if (i < size - 1) {
        temp = size - 1;
        size-= 8;
        goto startcode_check;
    }

    /* No startcode found, but if the last bytes vanish,
     * they may be part of a startcode whose end is not
     * in the current buffer. Return the offset of the
     * earliest element that may belong to a startcode. */
    for (i = size; i > FFMAX(size - 3, 0); i--)
        if (buf[i - 1])
            break;

    return i;

found_startcode:
    /* buf + i points to the byte preceding a startcode's 0x01.
     * Check whether it is a four-byte startcode. */

    i -= 1;
    if (i > 0 && buf[i - 1] == 0)
        i--;

    return i;
}
