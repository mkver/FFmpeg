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
#include "libavutil/macros.h"


#if AV_GCC_VERSION_AT_LEAST(2,96) || defined(__clang__)
# define av_likely(x)      __builtin_expect(!!(x), 1)
# define av_unlikely(x)    __builtin_expect(!!(x), 0)
#else
# define av_likely(x)      (x)
# define av_unlikely(x)    (x)
#endif


int ff_startcode_find_candidate_c(const uint8_t *buf, int size)
{
    const uint8_t *start = buf, *end = buf + size, *temp;

#define INITIALIZATION(mod) do {                                           \
        if (end - start <= mod + 1)                                        \
            goto near_end;                                                 \
        /* From this point on until the end of the MAIN_LOOP,              \
         * buf is the earliest possible position of a 0x00                 \
         * immediately preceding a startcode's 0x01, i.e.                  \
         * everything from start to buf (inclusive) is known               \
         * to not contain a startcode's 0x01. */                           \
        buf += 1;                                                          \
        temp = buf;                                                        \
        buf  = (const uint8_t *)FFALIGN((uintptr_t)buf, mod);              \
        /* Effective end for MAIN_LOOP in order not to overread. */        \
        end -= mod + 1;                                                    \
        goto startcode_check;                                              \
    } while (0)

#define READ(bitness) AV_RN ## bitness ## A
#define STARTCODE_CHECK(var, offset) do {                                  \
        if (var[2 - offset] > 1) {                                         \
            var += 3;                                                      \
        } else if (var[1 - offset]) {                                      \
            var += 2;                                                      \
        } else if (var[-offset] || var[2 - offset] == 0) {                 \
            var += 1;                                                      \
        } else {                                                           \
            buf  = var + 1 - offset;                                       \
            goto found_startcode;                                          \
        }                                                                  \
    } while (0)

    /* The MAIN_LOOP tries to read several bytes at once.
     * A startcode's 0x00 0x01 or 0x00 0x00 will be detected
     * by it if these bytes are contained within the bytes
     * read at once. */
#define MAIN_LOOP(bitness, mask1, mask2) do {                              \
        for (; buf < end; ) {                                              \
            uint ## bitness ## _t read = READ(bitness)(buf);               \
            buf += bitness / 8;                                            \
            if (~read & (read - mask1) & mask2) {                          \
                temp = buf - bitness / 8;                                  \
            startcode_check:                                               \
                do {                                                       \
                    STARTCODE_CHECK(temp, 1);                              \
                } while (temp < buf);                                      \
            }                                                              \
            /* If the 0x01 of a startcode is at position buf + 1,          \
             * the following check detects it.                             \
             * Because buf got incremented before entering this            \
             * loop, buf - 1 may be evaluated.                             \
             * Because temp + 1 < start + size, buf is always              \
             * <= the end of the buffer during the closing check           \
             * so that the pointer comparison is defined. */               \
        }                                                                  \
        /* Revert to the real end. */                                      \
        end += bitness / 8 + 1;                                            \
    } while (0)

#if HAVE_FAST_64BIT
    INITIALIZATION(8);
    MAIN_LOOP(64, 0x0101010101010101ULL, 0x8080808080808080ULL);
#else
    INITIALIZATION(4);
    MAIN_LOOP(32, 0x01010101U, 0x80808080U);
#endif

    /* For the end, buf is the earliest possible position
     * of a three-byte startcode's leading 0x00. This is
     * done in order not to run into the undefined behaviour
     * explained below. */
    buf--;
near_end:
    while (2 < end - buf) {
        /* The STARTCODE_CHECK in the MAIN_LOOP can't be used
         * to check whether the file ends with a startcode,
         * because for this buf would need to be end - 2.
         * But depending on the value of end[-1], buf might get
         * incremented by 3 and therefore be beyond end. But
         * pointer arithmetic beyond end is undefined behaviour.
         * So a STARTCODE_CHECK with a different offset is used
         * here: It detects whether buf points to the first 0x00
         * of a three-byte startcode. */
        STARTCODE_CHECK(buf, 0);
    }

    /* No startcode found, but if the last bytes vanish,
     * they may be part of a startcode whose end is not
     * in the current buffer. Return the offset of the
     * earliest element that may belong to a startcode. */
    for (buf = end; buf > (end - start > 3 ? end - 3 : start); buf--)
        if (buf[-1])
            break;

    return buf - start;

found_startcode:
    /* buf points to the byte preceding a startcode's 0x01.
     * Check whether it is a four-byte startcode. */

    buf -= 1;
    if (buf > start && buf[-1] == 0)
        buf--;

    return buf - start;
}
