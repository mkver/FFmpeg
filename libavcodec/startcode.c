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
#include "libavutil/common.h"
#include "libavutil/intreadwrite.h"

int ff_startcode_find_candidate_c(const uint8_t *buf, int size)
{
    const uint8_t *start = buf, *end = buf + size;

    /* The MAIN_LOOP tries to read several bytes at once.
     * A startcode's 0x00 0x01 or 0x00 0x00 will be detected
     * by it if these bytes are contained within the bytes
     * read at once.
     * It will always be made sure that at the beginning of
     * every iteration of the loop buf points at most to the
     * next possible position of a 0x00 immediately preceding
     * a startcode's 0x01. */
#define MAIN_LOOP(bitness, mask1, mask2) do {                              \
        for (; buf <= end - bitness / 8; ) {                               \
            if (!((~READ(bitness)(buf) & (READ(bitness)(buf) - mask1))     \
                                           & mask2)) {                     \
                buf += bitness / 8;                                        \
                continue;                                                  \
            }                                                              \
            /* No bounds check is necessary here as the bytes
             * just read are known to contain a zero. */                   \
            while (*buf)                                                   \
                buf++;                                                     \
            /* Go to the first nonzero. */                                 \
            do {                                                           \
                if (END_CHECK(bitness / 8))                                \
                    goto reached_end;                                      \
            } while (!*buf);                                               \
            STARTCODE_AND_ALIGNMENT_CHECK(bitness / 8);                    \
        }                                                                  \
        /* buf points at most to the earliest possible position of a 0x00
         * preceding a startcode's 0x01. The checks used at the end detect
         * whether *buf is a startcode's 0x01, so we may increment buf.
         * This also ensures that buf >= start + 2. */                     \
        buf++;                                                             \
    } while (0)

    /* If the 0x01 of a startcode is at position buf,
     * the following check detects it. Requires buf >= start + 2.
     * After the check buf points to the earliest possible
     * position of the next startcode's 0x01 (if no startcode
     * was found) and the check can be reapplied. */
#define STARTCODE_CHECK do {                                               \
        if (*buf > 1) {                                                    \
            buf += 3;                                                      \
        } else if (buf[-1]) {                                              \
            buf += 2;                                                      \
        } else if (buf[-2] || *buf == 0) {                                 \
            buf += 1;                                                      \
        } else                                                             \
            goto found_startcode;                                          \
    } while (0)

#if HAVE_FAST_UNALIGNED
#define READ(bitness) AV_RN ## bitness
#define END_CHECK(mod) (++buf == end)
    /* In contrast to the above STARTCODE_CHECK, this check
     * jumps over at most a startcode's initial 0x00
     * in accordance with the requirements of MAIN_LOOP. */
#define STARTCODE_AND_ALIGNMENT_CHECK(bitness) do {                        \
        if (*buf > 1) {                                                    \
            buf += 2;                                                      \
        } else if (buf[-2] == 0)                                           \
            goto found_startcode;                                          \
    } while (0)

    /* It is allowed to jump over a startcode's first 0x00.
     * Doing so here ensures that the requirement buf >= start + 2
     * of the above check is always fulfilled, because buf
     * also gets incremented in MAIN_LOOP, too. */
    buf++;

#if HAVE_FAST_64BIT
#if HAVE_BIGENDIAN
    MAIN_LOOP(64, 0x0002000200020002ULL, 0x8000800080008000ULL);
#else
    MAIN_LOOP(64, 0x0010010100100101ULL, 0x8008008080080080ULL);
#endif

#else

#if HAVE_BIGENDIAN
    MAIN_LOOP(32, 0x00020002U, 0x80008000U);
#else
    MAIN_LOOP(32, 0x00100101U, 0x80080080U);
#endif
#endif

    while (buf < end)
        STARTCODE_CHECK;

reached_end:

#else
#define READ(bitness) AV_RN ## bitness ## A
    /* In unaligned mode, we require that there is enough data
     * available so that MAKE_ALIGNED(++buf) is always contained
     * in the buffer. This implies that we do not overread
     * during STARTCODE_AND_ALIGNMENT_CHECK. */
#define END_CHECK(mod) (++buf > end - (mod))
#define MAKE_ALIGNED(mod) \
    ((const uint8_t *)((uintptr_t)(buf + (mod) - 1) / (mod) * (mod)))
#define STARTCODE_AND_ALIGNMENT_CHECK(mod) do {                            \
        make_aligned:                                                      \
            temp = MAKE_ALIGNED(mod);                                      \
        startcode_check:                                                   \
            do {                                                           \
                STARTCODE_CHECK;                                           \
            } while (buf <= temp);                                         \
            buf = temp;                                                    \
    } while (0)

    /* Given that the unaligned case requires special initialization
     * dependent on the bitness, another macro is used. */
#define ALIGNED(bitness, mask1, mask2) do {                                \
        const uint8_t *temp;                                               \
        buf += 2;                                                          \
        if (buf + bitness / 8 > end)                                       \
            goto reached_end;                                              \
        goto make_aligned;                                                 \
                                                                           \
        MAIN_LOOP(bitness, mask1, mask2);                                  \
                                                                           \
    reached_end:                                                           \
        if (buf < end) {                                                   \
            temp = end - 1;                                                \
            goto startcode_check;                                          \
        }                                                                  \
    } while (0)

#if HAVE_FAST_64BIT
#if HAVE_BIGENDIAN
    ALIGNED(64, 0x0002000200020002ULL, 0x8000800080008000ULL);
#else
    ALIGNED(64, 0x0010010100100101ULL, 0x8008008080080080ULL);
#endif

#else

#if HAVE_BIGENDIAN
    ALIGNED(32, 0x00020002U, 0x80008000U);
#else
    ALIGNED(32, 0x00100101U, 0x80080080U);
#endif
#endif
#endif

    /* No startcode found, but if the last bytes vanish,
     * they may be part of a startcode whose end is not
     * in the current buffer. Return the offset of the
     * earliest element that may belong to a startcode. */
    for (buf = end; buf > FFMAX(start, end - 3); buf--)
        if (buf[-1])
            break;

    return buf - start;

found_startcode:
    /* buf points to a startcode's 0x01. We have to
     * check whether it is a four-byte startcode. */

    buf -= 2;
    if (buf > start && buf[-1] == 0)
        buf--;

    return buf - start;
}
