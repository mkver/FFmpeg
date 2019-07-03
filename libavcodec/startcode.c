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

int ff_startcode_find_candidate_c(const uint8_t *buf, int size)
{
    const uint8_t *start = buf, *end = start + size;

    buf += 2;
    while (buf < end) {
        if (buf[0] > 1) {
            buf += 3;
        } else if (buf[-1]) {
            buf += 2;
        } else if (buf[-2] | (buf[0] == 0))
            buf++;
        else
            goto found_startcode;
    }

    for (buf = end; buf > (end - start > 3 ? end - 3 : start); buf--)
        if (buf[-1])
            break;

    return buf - start;

found_startcode:
    buf -= 2;
    if (start < buf && (buf[-1] == 0))
        buf--;
    return buf - start;
}
