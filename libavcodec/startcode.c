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
    int i, j;

    /* If the 0x01 of a startcode is at position i,
     * the following check detects it. Requires i >= 2.
     * This requirement is always fulfilled, because i
     * starts at 1 and gets incremented at least once more.*/
#define STARTCODE_CHECK do {                                                   \
        if (buf[i] > 1) {                                                      \
            i += 3;                                                            \
        } else if (buf[i - 1] > 0) {                                           \
            i += 2;                                                            \
        } else if (buf[i - 2] > 0 || buf[i] != 1) {                            \
            i += 1;                                                            \
        } else {                                                               \
            goto found_startcode;                                              \
        }                                                                      \
    } while (0)

#define MAIN_LOOP(bitness, mask1, mask2, aligned) do {                         \
        for (i = 1 + !aligned; i < size - bitness / 8 + 1; ) {                 \
            if (!((~READ(bitness)(buf + i) & (READ(bitness)(buf + i) - mask1)) \
                                           & mask2)) {                         \
                i += bitness / 8 + 2 * !aligned;                               \
                continue;                                                      \
            }                                                                  \
            if (buf[i] == 1)                                                   \
                goto startcode_check;                                          \
            /* No bounds check is necessary here as the bytes
             * just read are known to contain a zero. */                       \
            while (buf[i])                                                     \
                i++;                                                           \
            /* Go to the first nonzero */                                      \
            do {                                                               \
                if (++i == size)                                               \
                    goto reached_end;                                          \
            } while (!buf[i]);                                                 \
        startcode_check:                                                       \
            STARTCODE_AND_ALIGNMENT_CHECK(bitness);                            \
        }                                                                      \
    } while (0)


#if HAVE_FAST_UNALIGNED
#define READ(bitness) AV_RN ## bitness
#define STARTCODE_AND_ALIGNMENT_CHECK(bitness) do {                            \
        STARTCODE_CHECK;                                                       \
    } while (0)

#if HAVE_FAST_64BIT
    MAIN_LOOP(64, 0x0100010100100102ULL, 0x8080008080080080ULL, 0);
#else
    MAIN_LOOP(32, 0x01010101U, 0x80808080U);
#endif
#else 
#define READ(bitness) AV_RN ## bitness ## A
#define STARTCODE_AND_ALIGNMENT_CHECK(bitness) do {                            \
        j = i + bitness / 8 - (uintptr_t)(buf + i) % (bitness / 8);            \
    make_aligned:                                                              \
        do {                                                                   \
            STARTCODE_CHECK;                                                   \
        } while (i <= j);                                                      \
        i = j;                                                                 \
    } while (0)

    i = 2;

#if HAVE_FAST_64BIT
    MAIN_LOOP(64, 0x0010010100100101ULL, 0x8008008080080080ULL, 1);
#else
    MAIN_LOOP(32, 0x01010101U, 0x80808080U);
#endif
#endif

    if (i < size) {
        if (i == 1)
            i++;
        while (i < size)
            STARTCODE_CHECK;
    }


reached_end:
    /* No startcode found, but if the last bytes vanish,
     * they may be part of a startcode whose end is not
     * in the current buffer. Return the offset of the
     * earliest element that may belong to a startcode. */

    j = size > 3 ? size - 3 : 0;
    for (i = size; i > j; i--)
        if (buf[i - 1])
            break;

    return i;

found_startcode:
    /* i is the position of a startcode's 0x01. We have to
     * check whether it is a four-byte startcode. */

     i -= 2;
     if (i > 0 && buf[i - 1] == 0)
         i--;

     return i;
}
