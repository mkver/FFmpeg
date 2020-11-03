/*
 * VC-1 and WMV3 decoder
 * copyright (c) 2006 Konstantin Shishkov
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

#ifndef AVCODEC_VC1ACDATA_H
#define AVCODEC_VC1ACDATA_H

#include <stdint.h>

#include "vc1data.h"

static const uint8_t vc1_delta_level_table[AC_MODES][31] = {
{
      19,    15,    12,    11,     6,     5,     4,     4,     4,     4,
       3,     3,     3,     3,     3,     3,     2,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1
},
{
      23,    11,     8,     7,     5,     5,     4,     4,     3,     3,
       3,     3,     2,     2,     2,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1
},
{
      16,    11,     8,     7,     5,     4,     4,     3,     3,     3,
       3,     3,     3,     3,     2,     2,     1,     1,     1,     1,
       1
},
{
      14,     9,     5,     4,     4,     4,     3,     3,     3,     3,
       3,     3,     3,     2,     2,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,
       1
},
{
      27,    10,     5,     4,     3,     3,     3,     3,     2,     2,
       1,     1,     1,     1,     1
},
{
      12,     6,     4,     3,     3,     3,     3,     2,     2,     2,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1
},
{
      56,    20,    10,     7,     6,     5,     4,     3,     3,     3,
       2,     2,     2,     2,     1
},
{
      32,    13,     8,     6,     5,     4,     4,     3,     3,     3,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     1,     1
}
};

static const uint8_t vc1_last_delta_level_table[AC_MODES][44] = {
{
       6,     5,     4,     4,     3,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1
},
{
       9,     5,     4,     4,     3,     3,     3,     2,     2,     2,
       2,     2,     2,     2,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1
},
{
       4,     4,     3,     3,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1
},
{
       5,     4,     3,     3,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1
},
{
       8,     3,     2,     2,     2,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1
},
{
       3,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1
},
{
       4,     3,     3,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     1,     1
},
{
       4,     3,     3,     3,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     1,
       1
}
};

static const uint8_t vc1_delta_run_table[AC_MODES][57] = {
{
      -1,    30,    17,    15,     9,     5,     4,     3,     3,     3,
       3,     3,     2,     1,     1,     1,     0,     0,     0,
       0
},
{
      -1,    26,    16,    11,     7,     5,     3,     3,     2,     1,
       1,     1,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0
},
{
      -1,    20,    15,    13,     6,     4,     3,     3,     2,     1,
       1,     1,     0,     0,     0,     0,     0
},
{
      -1,    29,    15,    12,     5,     2,     1,     1,     1,     1,
       0,     0,     0,     0,     0
},
{
      -1,    14,     9,     7,     3,     2,     1,     1,     1,     1,
       1,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0
},
{
      -1,    26,    10,     6,     2,     1,     1,     0,     0,     0,
       0,     0,     0
},
{
      -1,    14,    13,     9,     6,     5,     4,     3,     2,     2,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0
},
{
      -1,    24,    22,     9,     6,     4,     3,     2,     2,     1,
       1,     1,     1,     1,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0
}
};

static const uint8_t vc1_last_delta_run_table[AC_MODES][10] = {
{
      -1,    37,    15,     4,     3,     1,     0
},
{
      -1,    36,    14,     6,     3,     1,     0,     0,     0,
       0
},
{
      -1,    26,    13,     3,     1
},
{
      -1,    43,    15,     3,     1,     0
},
{
      -1,    20,     6,     1,     0,     0,     0,     0,     0
},
{
      -1,    40,     1,     0
},
{
      -1,    16,    14,     2,     0
},
{
      -1,    30,    28,     3,     0
}
};

#endif /* AVCODEC_VC1ACDATA_H */
