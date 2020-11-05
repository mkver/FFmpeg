/*
 * VC-1 and WMV3 decoder
 * copyright (c) 2011 Mashiat Sarker Shakkhar
 * copyright (c) 2006 Konstantin Shishkov
 * (c) 2005 anonymous, Alex Beregszaszi, Michael Niedermayer
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
 * VC-1 tables.
 */

#include "avcodec.h"
#include "vc1.h"
#include "vc1data.h"

const int ff_vc1_ttfrm_to_tt[4] = { TT_8X8, TT_8X4, TT_4X8, TT_4X4 };

/** MV P mode - the 5th element is only used for mode 1 */
const uint8_t ff_vc1_mv_pmode_table[2][5] = {
    { MV_PMODE_1MV_HPEL_BILIN, MV_PMODE_1MV, MV_PMODE_1MV_HPEL, MV_PMODE_INTENSITY_COMP, MV_PMODE_MIXED_MV },
    { MV_PMODE_1MV, MV_PMODE_MIXED_MV, MV_PMODE_1MV_HPEL, MV_PMODE_INTENSITY_COMP, MV_PMODE_1MV_HPEL_BILIN }
};
const uint8_t ff_vc1_mv_pmode_table2[2][4] = {
    { MV_PMODE_1MV_HPEL_BILIN, MV_PMODE_1MV, MV_PMODE_1MV_HPEL, MV_PMODE_MIXED_MV },
    { MV_PMODE_1MV, MV_PMODE_MIXED_MV, MV_PMODE_1MV_HPEL, MV_PMODE_1MV_HPEL_BILIN }
};

const int ff_vc1_fps_nr[7] = { 24, 25, 30, 50, 60, 48, 72 },
          ff_vc1_fps_dr[2] = { 1000, 1001 };
const uint8_t ff_vc1_pquant_table[3][32] = {
    /* Implicit quantizer */
    {  0,  1,  2,  3,  4,  5,  6,  7,  8,  6,  7,  8,  9, 10, 11, 12,
      13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 29, 31 },
    /* Explicit quantizer, pquantizer uniform */
    {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 },
    /* Explicit quantizer, pquantizer non-uniform */
    {  0,  1,  1,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13,
      14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 31 }
};

/** @name VC-1 VLC tables and defines
 *  @todo TODO move this into the context
 */
//@{
#define VC1_BFRACTION_VLC_BITS 7
VLC ff_vc1_bfraction_vlc;
#define VC1_IMODE_VLC_BITS 4
VLC ff_vc1_imode_vlc;
#define VC1_NORM2_VLC_BITS 3
VLC ff_vc1_norm2_vlc;
#define VC1_NORM6_VLC_BITS 9
VLC ff_vc1_norm6_vlc;
/* Could be optimized, one table only needs 8 bits */
#define VC1_TTMB_VLC_BITS 9 //12
VLC ff_vc1_ttmb_vlc[3];
#define VC1_MV_DIFF_VLC_BITS 9 //15
VLC ff_vc1_mv_diff_vlc[4];
#define VC1_CBPCY_P_VLC_BITS 9 //14
VLC ff_vc1_cbpcy_p_vlc[4];
#define VC1_ICBPCY_VLC_BITS 9
VLC ff_vc1_icbpcy_vlc[8];
#define VC1_4MV_BLOCK_PATTERN_VLC_BITS 6
VLC ff_vc1_4mv_block_pattern_vlc[4];
#define VC1_2MV_BLOCK_PATTERN_VLC_BITS 3
VLC ff_vc1_2mv_block_pattern_vlc[4];
#define VC1_TTBLK_VLC_BITS 5
VLC ff_vc1_ttblk_vlc[3];
#define VC1_SUBBLKPAT_VLC_BITS 6
VLC ff_vc1_subblkpat_vlc[3];
#define VC1_INTFR_4MV_MBMODE_VLC_BITS 9
VLC ff_vc1_intfr_4mv_mbmode_vlc[4];
#define VC1_INTFR_NON4MV_MBMODE_VLC_BITS 6
VLC ff_vc1_intfr_non4mv_mbmode_vlc[4];
#define VC1_IF_MMV_MBMODE_VLC_BITS 5
VLC ff_vc1_if_mmv_mbmode_vlc[8];
#define VC1_IF_1MV_MBMODE_VLC_BITS 5
VLC ff_vc1_if_1mv_mbmode_vlc[8];
#define VC1_1REF_MVDATA_VLC_BITS 9
VLC ff_vc1_1ref_mvdata_vlc[4];
#define VC1_2REF_MVDATA_VLC_BITS 9
VLC ff_vc1_2ref_mvdata_vlc[8];

VLC ff_vc1_ac_coeff_table[8];

#define VC1_IF_MBMODE_VLC_BITS 5    // as a placeholder for VC1_IF_MMV_MBMODE_VLC_BITS
                                    // or VC1_IF_1MV_MBMODE_VLC_BITS since they are the same
//@}


#if B_FRACTION_DEN == 840 // original bfraction from vc9data.h, not conforming to standard
/* bfraction is fractional, we scale to the GCD 3*5*7*8 = 840 */
const int16_t ff_vc1_bfraction_lut[23] = {
    420 /*1/2*/, 280 /*1/3*/, 560 /*2/3*/, 210 /*1/4*/,
    630 /*3/4*/, 168 /*1/5*/, 336 /*2/5*/,
    504 /*3/5*/, 672 /*4/5*/, 140 /*1/6*/, 700 /*5/6*/,
    120 /*1/7*/, 240 /*2/7*/, 360 /*3/7*/, 480 /*4/7*/,
    600 /*5/7*/, 720 /*6/7*/, 105 /*1/8*/, 315 /*3/8*/,
    525 /*5/8*/, 735 /*7/8*/,
    -1 /*inv.*/, 0 /*BI fm*/
};
#else
/* pre-computed scales for all bfractions and base=256 */
const int16_t ff_vc1_bfraction_lut[23] = {
    128 /*1/2*/,  85 /*1/3*/, 170 /*2/3*/,  64 /*1/4*/,
    192 /*3/4*/,  51 /*1/5*/, 102 /*2/5*/,
    153 /*3/5*/, 204 /*4/5*/,  43 /*1/6*/, 215 /*5/6*/,
     37 /*1/7*/,  74 /*2/7*/, 111 /*3/7*/, 148 /*4/7*/,
    185 /*5/7*/, 222 /*6/7*/,  32 /*1/8*/,  96 /*3/8*/,
    160 /*5/8*/, 224 /*7/8*/,
    -1 /*inv.*/, 0 /*BI fm*/
};
#endif

const uint8_t ff_vc1_bfraction_bits[23] = {
    3, 3, 3, 3,
    3, 3, 3,
    7, 7, 7, 7,
    7, 7, 7, 7,
    7, 7, 7, 7,
    7, 7,
    7, 7
};
const uint8_t ff_vc1_bfraction_codes[23] = {
      0,   1,   2,   3,
      4,   5,   6,
    112, 113, 114, 115,
    116, 117, 118, 119,
    120, 121, 122, 123,
    124, 125,
    126, 127
};

//Same as H.264
const AVRational ff_vc1_pixel_aspect[16] = {
    {   0,  1 },
    {   1,  1 },
    {  12, 11 },
    {  10, 11 },
    {  16, 11 },
    {  40, 33 },
    {  24, 11 },
    {  20, 11 },
    {  32, 11 },
    {  80, 33 },
    {  18, 11 },
    {  15, 11 },
    {  64, 33 },
    { 160, 99 },
    {   0,  1 },
    {   0,  1 }
};

/* BitPlane IMODE - such a small table... */
const uint8_t ff_vc1_imode_codes[7] = {
    0, 2, 1, 3, 1, 2, 3
};
const uint8_t ff_vc1_imode_bits[7] = {
    4, 2, 3, 2, 4, 3, 3
};

/* Normal-2 imode */
const uint8_t ff_vc1_norm2_codes[4] = {
    0, 4, 5, 3
};
const uint8_t ff_vc1_norm2_bits[4] = {
    1, 3, 3, 2
};

const uint16_t ff_vc1_norm6_codes[64] = {
    0x001, 0x002, 0x003, 0x000, 0x004, 0x001, 0x002, 0x047, 0x005, 0x003, 0x004, 0x04B, 0x005, 0x04D, 0x04E, 0x30E,
    0x006, 0x006, 0x007, 0x053, 0x008, 0x055, 0x056, 0x30D, 0x009, 0x059, 0x05A, 0x30C, 0x05C, 0x30B, 0x30A, 0x037,
    0x007, 0x00A, 0x00B, 0x043, 0x00C, 0x045, 0x046, 0x309, 0x00D, 0x049, 0x04A, 0x308, 0x04C, 0x307, 0x306, 0x036,
    0x00E, 0x051, 0x052, 0x305, 0x054, 0x304, 0x303, 0x035, 0x058, 0x302, 0x301, 0x034, 0x300, 0x033, 0x032, 0x007,
};

const uint8_t ff_vc1_norm6_bits[64] = {
    1,  4,  4,  8,  4,  8,  8, 10,  4,  8,  8, 10,  8, 10, 10, 13,
    4,  8,  8, 10,  8, 10, 10, 13,  8, 10, 10, 13, 10, 13, 13,  9,
    4,  8,  8, 10,  8, 10, 10, 13,  8, 10, 10, 13, 10, 13, 13,  9,
    8, 10, 10, 13, 10, 13, 13,  9, 10, 13, 13,  9, 13,  9,  9,  6,
};

/* 4MV Block pattern VLC tables */
const uint8_t ff_vc1_4mv_block_pattern_codes[4][16] = {
    { 14, 58, 59, 25, 12, 26, 15, 15, 13, 24, 27,  0, 28,  1,  2,  2 },
    {  8, 18, 19,  4, 20,  5, 30, 11, 21, 31,  6, 12,  7, 13, 14,  0 },
    { 15,  6,  7,  2,  8,  3, 28,  9, 10, 29,  4, 11,  5, 12, 13,  0 },
    {  0, 11, 12,  4, 13,  5, 30, 16, 14, 31,  6, 17,  7, 18, 19, 10 }
};
const uint8_t ff_vc1_4mv_block_pattern_bits[4][16] = {
    { 5, 6, 6, 5, 5, 5, 5, 4, 5, 5, 5, 3, 5, 3, 3, 2 },
    { 4, 5, 5, 4, 5, 4, 5, 4, 5, 5, 4, 4, 4, 4, 4, 2 },
    { 4, 4, 4, 4, 4, 4, 5, 4, 4, 5, 4, 4, 4, 4, 4, 3 },
    { 2, 4, 4, 4, 4, 4, 5, 5, 4, 5, 4, 5, 4, 5, 5, 4 }
};

/* 2MV Block pattern VLC tables */
const uint8_t ff_vc1_2mv_block_pattern_codes[4][4] = {
    { 2, 1, 0, 3 }, { 1, 0, 2, 3 }, { 2, 0, 3, 1 }, { 1, 3, 2, 0 }
};

const uint8_t ff_vc1_2mv_block_pattern_bits[4][4] = {
    { 2, 2, 2, 2 }, { 1, 2, 3, 3 }, { 3, 2, 3, 1 }, { 1, 3, 3, 2 }
};

/* Interlaced frame picture 4MV MBMODE VLC tables (tables 160-163) */
const uint8_t ff_vc1_intfr_4mv_mbmode_tabs[4][15][2] = {
    {
        { MV_PMODE_INTFR_1MV | MVDIFF,  2 },
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX | RESIDUAL,  2 },
        { MV_PMODE_INTFR_1MV | FIELDTX | RESIDUAL,  6 },
        { MV_PMODE_INTFR_4MV,  8 },
        { MV_PMODE_INTFR_4MV | FIELDTX | RESIDUAL,  8 },
        { MV_PMODE_INTFR_4MV | RESIDUAL,  7 },
        { MV_PMODE_INTFR_1MV | FIELDTX | MVDIFF | RESIDUAL,  5 },
        { MV_PMODE_INTFR_INTRA,  5 },
        { MV_PMODE_INTFR_4MV_FIELD | FIELDTX | RESIDUAL,  5 },
        { MV_PMODE_INTFR_2MV_FIELD | RESIDUAL,  4 },
        { MV_PMODE_INTFR_1MV | MVDIFF | RESIDUAL,  5 },
        { MV_PMODE_INTFR_4MV_FIELD | RESIDUAL,  7 },
        { MV_PMODE_INTFR_4MV_FIELD | FIELDTX,  7 },
        { MV_PMODE_INTFR_1MV | RESIDUAL,  6 },
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX,  2 },
    },
    {
        { MV_PMODE_INTFR_1MV | MVDIFF,  3 },
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX | RESIDUAL,  3 },
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX,  3 },
        { MV_PMODE_INTFR_1MV | MVDIFF | RESIDUAL,  3 },
        { MV_PMODE_INTFR_4MV_FIELD | RESIDUAL,  4 },
        { MV_PMODE_INTFR_4MV_FIELD | FIELDTX | RESIDUAL,  5 },
        { MV_PMODE_INTFR_INTRA,  7 },
        { MV_PMODE_INTFR_4MV_FIELD | FIELDTX,  7 },
        { MV_PMODE_INTFR_4MV | FIELDTX | RESIDUAL,  6 },
        { MV_PMODE_INTFR_4MV | RESIDUAL,  4 },
        { MV_PMODE_INTFR_4MV,  6 },
        { MV_PMODE_INTFR_1MV | FIELDTX | MVDIFF | RESIDUAL,  6 },
        { MV_PMODE_INTFR_1MV | FIELDTX | RESIDUAL,  5 },
        { MV_PMODE_INTFR_2MV_FIELD | RESIDUAL,  3 },
        { MV_PMODE_INTFR_1MV | RESIDUAL,  3 },
    },
    {
        { MV_PMODE_INTFR_4MV_FIELD | FIELDTX | RESIDUAL,  2 },
        { MV_PMODE_INTFR_4MV | FIELDTX | RESIDUAL,  5 },
        { MV_PMODE_INTFR_1MV | RESIDUAL,  5 },
        { MV_PMODE_INTFR_4MV,  7 },
        { MV_PMODE_INTFR_1MV | FIELDTX | RESIDUAL,  7 },
        { MV_PMODE_INTFR_4MV_FIELD | FIELDTX,  6 },
        { MV_PMODE_INTFR_INTRA,  5 },
        { MV_PMODE_INTFR_2MV_FIELD | RESIDUAL,  4 },
        { MV_PMODE_INTFR_4MV | RESIDUAL,  5 },
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX,  5 },
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX | RESIDUAL,  2 },
        { MV_PMODE_INTFR_1MV | FIELDTX | MVDIFF | RESIDUAL,  3 },
        { MV_PMODE_INTFR_1MV | MVDIFF,  5 },
        { MV_PMODE_INTFR_4MV_FIELD | RESIDUAL,  5 },
        { MV_PMODE_INTFR_1MV | MVDIFF | RESIDUAL,  4 },
    },
    {
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX,  2 },
        { MV_PMODE_INTFR_1MV | RESIDUAL,  3 },
        { MV_PMODE_INTFR_4MV_FIELD | FIELDTX,  9 },
        { MV_PMODE_INTFR_1MV | FIELDTX | RESIDUAL,  9 },
        { MV_PMODE_INTFR_4MV | RESIDUAL,  8 },
        { MV_PMODE_INTFR_4MV,  8 },
        { MV_PMODE_INTFR_1MV | FIELDTX | MVDIFF | RESIDUAL,  9 },
        { MV_PMODE_INTFR_4MV_FIELD | RESIDUAL, 10 },
        { MV_PMODE_INTFR_4MV_FIELD | FIELDTX | RESIDUAL, 11 },
        { MV_PMODE_INTFR_INTRA, 12 },
        { MV_PMODE_INTFR_4MV | FIELDTX | RESIDUAL, 12 },
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX | RESIDUAL,  6 },
        { MV_PMODE_INTFR_2MV_FIELD | RESIDUAL,  5 },
        { MV_PMODE_INTFR_1MV | MVDIFF | RESIDUAL,  4 },
        { MV_PMODE_INTFR_1MV | MVDIFF,  1 },
    },
};

/* Interlaced frame picture NON-4MV MBMODE VLC tables (tables 164-167) */
const uint8_t ff_vc1_intfr_non4mv_mbmode_tabs[4][9][2] = {
    {
        { MV_PMODE_INTFR_1MV | MVDIFF,  2 },
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX | RESIDUAL,  2 },
        { MV_PMODE_INTFR_1MV | FIELDTX | RESIDUAL,  5 },
        { MV_PMODE_INTFR_1MV | RESIDUAL,  5 },
        { MV_PMODE_INTFR_1MV | MVDIFF | RESIDUAL,  4 },
        { MV_PMODE_INTFR_2MV_FIELD | RESIDUAL,  4 },
        { MV_PMODE_INTFR_1MV | FIELDTX | MVDIFF | RESIDUAL,  5 },
        { MV_PMODE_INTFR_INTRA,  5 },
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX,  2 },
    },
    {
        { MV_PMODE_INTFR_1MV | FIELDTX | MVDIFF | RESIDUAL,  4 },
        { MV_PMODE_INTFR_INTRA,  6 },
        { MV_PMODE_INTFR_1MV | MVDIFF,  6 },
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX,  5 },
        { MV_PMODE_INTFR_1MV | FIELDTX | RESIDUAL,  3 },
        { MV_PMODE_INTFR_2MV_FIELD | RESIDUAL,  2 },
        { MV_PMODE_INTFR_1MV | RESIDUAL,  2 },
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX | RESIDUAL,  3 },
        { MV_PMODE_INTFR_1MV | MVDIFF | RESIDUAL,  3 },
    },
    {
        { MV_PMODE_INTFR_1MV | FIELDTX | MVDIFF | RESIDUAL,  2 },
        { MV_PMODE_INTFR_1MV | MVDIFF | RESIDUAL,  2 },
        { MV_PMODE_INTFR_2MV_FIELD | RESIDUAL,  4 },
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX,  4 },
        { MV_PMODE_INTFR_1MV | MVDIFF,  4 },
        { MV_PMODE_INTFR_1MV | FIELDTX | RESIDUAL,  6 },
        { MV_PMODE_INTFR_INTRA,  6 },
        { MV_PMODE_INTFR_1MV | RESIDUAL,  5 },
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX | RESIDUAL,  2 },
    },
    {
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX,  2 },
        { MV_PMODE_INTFR_1MV | RESIDUAL,  3 },
        { MV_PMODE_INTFR_INTRA,  8 },
        { MV_PMODE_INTFR_1MV | FIELDTX | MVDIFF | RESIDUAL,  8 },
        { MV_PMODE_INTFR_1MV | FIELDTX | RESIDUAL,  7 },
        { MV_PMODE_INTFR_2MV_FIELD | FIELDTX | RESIDUAL,  6 },
        { MV_PMODE_INTFR_2MV_FIELD | RESIDUAL,  5 },
        { MV_PMODE_INTFR_1MV | MVDIFF | RESIDUAL,  4 },
        { MV_PMODE_INTFR_1MV | MVDIFF,  1 },
    },
};

/* Interlaced field picture MBMODE VLC tables (p. 356 - 11.4.1, 11.4.2) */
/* mixed-MV */
const uint8_t ff_vc1_if_mmv_mbmode_codes[8][8] = {
    { 16, 17,  3,  3,  0,  5,  9,  2 },
    {  8,  9,  3,  6,  7,  0,  5,  2 },
    { 16, 17,  5,  3,  0,  3,  9,  2 },
    { 56, 57, 15,  4,  5,  6, 29,  0 },
    { 52, 53, 27, 14, 15,  2, 12,  0 },
    { 56, 57, 29,  5,  6,  0, 15,  4 },
    { 16, 17,  6,  7,  0,  1,  9,  5 },
    { 56, 57,  0,  5,  6, 29,  4, 15 }
};
const uint8_t ff_vc1_if_mmv_mbmode_bits[8][8] = {
    { 6, 6, 2, 3, 2, 4, 5, 2 },
    { 5, 5, 3, 3, 3, 2, 4, 2 },
    { 6, 6, 4, 3, 2, 2, 5, 2 },
    { 6, 6, 4, 3, 3, 3, 5, 1 },
    { 6, 6, 5, 4, 4, 2, 4, 1 },
    { 6, 6, 5, 3, 3, 1, 4, 3 },
    { 5, 5, 3, 3, 2, 2, 4, 3 },
    { 6, 6, 1, 3, 3, 5, 3, 4 }
};
/* 1MV */
const uint8_t ff_vc1_if_1mv_mbmode_codes[8][6] = {
    {  0,  1,  1,  1,  1,  1 },
    {  0,  1,  1,  1,  1,  1 },
    { 16, 17,  3,  0,  9,  5 },
    { 20, 21,  3, 11,  0,  4 },
    {  4,  5,  2,  3,  3,  0 },
    {  4,  5,  3,  2,  0,  3 },
    {  0,  1,  1,  1,  1,  1 },
    { 16, 17,  9,  5,  3,  0 }
};
const uint8_t ff_vc1_if_1mv_mbmode_bits[8][6] = {
    { 5, 5, 1, 3, 2, 4 },
    { 5, 5, 1, 2, 3, 4 },
    { 5, 5, 2, 1, 4, 3 },
    { 5, 5, 2, 4, 1, 3 },
    { 4, 4, 2, 3, 2, 2 },
    { 4, 4, 3, 2, 2, 2 },
    { 5, 5, 3, 4, 1, 2 },
    { 5, 5, 4, 3, 2, 1 }
};

/* Interlaced frame/field picture MVDATA VLC tables */

/* 1-reference tables */
const uint8_t ff_vc1_1ref_mvdata_tabs[4][72][2] = {
    {
        { 0x10,  2 }, { 0x13,  5 }, { 0x24,  6 }, { 0x53,  8 }, { 0x26,  8 },
        { 0x32,  7 }, { 0x20,  4 }, { 0x05,  5 }, { 0x37,  8 }, { 0x54,  8 },
        { 0x35,  7 }, { 0x21,  6 }, { 0x58,  8 }, { 0x36,  8 }, { 0x64, 10 },
        { 0x63, 11 }, { 0x73, 16 }, { 0x74, 16 }, { 0x75, 16 }, { 0x76, 16 },
        { 0x77, 16 }, { 0x78, 16 }, {    0, 16 }, { 0x71, 17 }, { 0x72, 17 },
        { 0x61, 13 }, { 0x62, 12 }, { 0x51, 10 }, { 0x65, 10 }, { 0x41,  8 },
        { 0x14,  6 }, { 0x23,  6 }, { 0x50,  7 }, { 0x45,  8 }, { 0x31,  8 },
        { 0x11,  4 }, { 0x04,  5 }, { 0x22,  6 }, { 0x40,  6 }, { 0x01,  3 },
        { 0x02,  4 }, { 0x06,  6 }, { 0x33,  7 }, { 0x16,  8 }, { 0x67,  9 },
        { 0x18,  9 }, { 0x12,  5 }, { 0x08,  7 }, { 0x34,  7 }, { 0x30,  6 },
        { 0x43,  8 }, { 0x44,  8 }, { 0x07,  7 }, { 0x25,  7 }, { 0x38,  9 },
        { 0x60,  9 }, { 0x17,  9 }, { 0x56,  9 }, { 0x03,  5 }, { 0x15,  7 },
        { 0x28,  9 }, { 0x47,  9 }, { 0x27,  9 }, { 0x57,  9 }, { 0x68,  9 },
        { 0x46,  9 }, { 0x55,  9 }, { 0x70,  9 }, { 0x66, 10 }, { 0x52, 10 },
        { 0x48,  9 }, { 0x42,  8 },
    },
    {
        { 0x23,  5 }, { 0x05,  5 }, { 0x25,  6 }, { 0x44,  8 }, { 0x55, 10 },
        { 0x47, 10 }, { 0x45,  9 }, { 0x43,  8 }, { 0x53,  9 }, { 0x51,  9 },
        { 0x07,  8 }, { 0x36,  9 }, { 0x17, 10 }, { 0x28, 11 }, { 0x18, 12 },
        { 0x57, 12 }, { 0x41,  8 }, { 0x52,  9 }, { 0x37,  9 }, { 0x34,  7 },
        { 0x32,  7 }, { 0x02,  3 }, { 0x11,  4 }, { 0x40,  5 }, { 0x15,  7 },
        { 0x46, 10 }, { 0x54, 10 }, { 0x27,  9 }, { 0x26,  8 }, { 0x22,  6 },
        { 0x30,  4 }, { 0x03,  4 }, { 0x13,  5 }, { 0x31,  7 }, { 0x06,  7 },
        { 0x33,  7 }, { 0x60,  8 }, { 0x66, 11 }, { 0x62, 11 }, { 0x64, 12 },
        { 0x67, 14 }, { 0x48, 14 }, { 0x38, 13 }, { 0x56, 11 }, { 0x42,  9 },
        { 0x20,  4 }, { 0x21,  6 }, { 0x14,  6 }, { 0x24,  6 }, { 0x35,  8 },
        { 0x16,  9 }, { 0x08, 10 }, { 0x63, 11 }, { 0x61, 12 }, { 0x73, 19 },
        { 0x74, 19 }, { 0x75, 19 }, { 0x76, 19 }, { 0x77, 19 }, { 0x78, 19 },
        {    0, 19 }, { 0x71, 20 }, { 0x72, 20 }, { 0x68, 16 }, { 0x70, 15 },
        { 0x58, 14 }, { 0x65, 13 }, { 0x50,  7 }, { 0x04,  5 }, { 0x12,  5 },
        { 0x10,  3 }, { 0x01,  3 },
    },
    {
        { 0x11,  4 }, { 0x30,  5 }, { 0x23,  6 }, { 0x15,  6 }, { 0x41,  9 },
        { 0x48, 12 }, { 0x70, 13 }, { 0x73, 18 }, { 0x74, 18 }, { 0x75, 18 },
        { 0x76, 18 }, { 0x77, 18 }, { 0x78, 18 }, {    0, 18 }, { 0x71, 19 },
        { 0x72, 19 }, { 0x58, 15 }, { 0x66, 15 }, { 0x68, 15 }, { 0x47, 11 },
        { 0x37, 10 }, { 0x35,  9 }, { 0x42,  9 }, { 0x33,  8 }, { 0x18,  8 },
        { 0x24,  6 }, { 0x12,  5 }, { 0x25,  7 }, { 0x53, 11 }, { 0x45, 11 },
        { 0x62, 10 }, { 0x60,  9 }, { 0x27,  8 }, { 0x22,  7 }, { 0x21,  7 },
        { 0x05,  5 }, { 0x01,  3 }, { 0x02,  4 }, { 0x03,  4 }, { 0x10,  2 },
        { 0x06,  6 }, { 0x38, 11 }, { 0x61, 12 }, { 0x64, 12 }, { 0x52, 11 },
        { 0x36, 11 }, { 0x51, 11 }, { 0x65, 12 }, { 0x56, 12 }, { 0x54, 11 },
        { 0x63, 11 }, { 0x26,  8 }, { 0x16,  7 }, { 0x13,  5 }, { 0x04,  4 },
        { 0x20,  4 }, { 0x14,  5 }, { 0x08,  9 }, { 0x28,  9 }, { 0x43, 10 },
        { 0x44, 10 }, { 0x32,  9 }, { 0x40,  7 }, { 0x50,  8 }, { 0x17,  8 },
        { 0x34,  9 }, { 0x57, 13 }, { 0x67, 13 }, { 0x46, 12 }, { 0x55, 11 },
        { 0x31, 10 }, { 0x07,  8 },
    },
    {
        { 0x04,  4 }, { 0x02,  4 }, { 0x44,  6 }, { 0x43,  6 }, { 0x06,  5 },
        { 0x30,  4 }, { 0x03,  4 }, { 0x25,  7 }, { 0x35,  7 }, { 0x24,  6 },
        { 0x52,  8 }, { 0x73, 13 }, { 0x74, 13 }, { 0x75, 13 }, { 0x76, 13 },
        { 0x77, 13 }, { 0x78, 13 }, {    0, 13 }, { 0x71, 14 }, { 0x72, 14 },
        { 0x58, 10 }, { 0x68,  9 }, { 0x45,  7 }, { 0x14,  6 }, { 0x10,  3 },
        { 0x32,  6 }, { 0x34,  6 }, { 0x11,  5 }, { 0x36,  8 }, { 0x61,  8 },
        { 0x53,  8 }, { 0x26,  8 }, { 0x16,  7 }, { 0x28,  8 }, { 0x66,  8 },
        { 0x41,  7 }, { 0x08,  7 }, { 0x60,  7 }, { 0x70,  9 }, { 0x48,  9 },
        { 0x46,  8 }, { 0x20,  4 }, { 0x21,  6 }, { 0x33,  6 }, { 0x05,  5 },
        { 0x42,  7 }, { 0x63,  9 }, { 0x57,  9 }, { 0x17,  8 }, { 0x50,  6 },
        { 0x22,  6 }, { 0x27,  8 }, { 0x56,  8 }, { 0x15,  7 }, { 0x01,  4 },
        { 0x40,  5 }, { 0x37,  9 }, { 0x64,  9 }, { 0x62,  8 }, { 0x31,  7 },
        { 0x13,  6 }, { 0x54,  8 }, { 0x55,  8 }, { 0x18,  8 }, { 0x65,  9 },
        { 0x67,  9 }, { 0x23,  6 }, { 0x12,  6 }, { 0x51,  8 }, { 0x47,  9 },
        { 0x38,  9 }, { 0x07,  7 },
    },
};

/* 2-reference tables */
const uint8_t ff_vc1_2ref_mvdata_tabs[8][126][2] = { /* tables 132 - 139 */
    {
        { 0x04,  5 }, { 0x80,  7 }, { 0x67, 10 }, { 0x82, 10 }, { 0xB2, 11 },
        { 0xD6, 13 }, { 0xA8, 13 }, { 0x68, 12 }, { 0x92, 10 }, { 0x37, 10 },
        { 0x98, 12 }, { 0xC7, 12 }, { 0xC2, 13 }, { 0xD1, 13 }, { 0xB8, 12 },
        { 0x63,  9 }, { 0x54,  7 }, { 0x34,  7 }, { 0x32,  6 }, { 0x75,  9 },
        { 0xC0,  9 }, { 0xD0,  9 }, { 0x61,  9 }, { 0x24,  7 }, { 0x21,  5 },
        { 0x12,  5 }, { 0x40,  5 }, { 0x53,  7 }, { 0xB0,  8 }, { 0x95, 11 },
        { 0x28, 11 }, { 0xB6, 12 }, { 0x87, 12 }, { 0xC3, 11 }, { 0x66, 10 },
        { 0x47, 10 }, { 0x23,  6 }, { 0x05,  6 }, { 0x46,  9 }, { 0x57,  9 },
        { 0xB1, 12 }, { 0xD4, 12 }, { 0x96, 11 }, { 0x81, 10 }, { 0x74,  9 },
        { 0x25,  8 }, { 0xA0,  8 }, { 0x70,  6 }, { 0x55,  8 }, { 0xD2, 13 },
        { 0xC8, 13 }, { 0xA4, 12 }, { 0xA3, 11 }, { 0xB5, 12 }, { 0xA1, 12 },
        { 0xA2, 11 }, { 0x36,  9 }, { 0x51,  7 }, { 0x50,  5 }, { 0x07,  8 },
        { 0x65, 10 }, { 0x83, 11 }, { 0x84, 11 }, { 0x27, 10 }, {    0, 13 },
        { 0xD8, 13 }, { 0x97, 12 }, { 0x85, 11 }, { 0x44,  8 }, { 0x73,  9 },
        { 0x26,  9 }, { 0x06,  7 }, { 0x42,  8 }, { 0xA7, 12 }, { 0xB4, 12 },
        { 0xD7, 13 }, { 0xD5, 13 }, { 0x48, 12 }, { 0x62, 10 }, { 0xA5, 12 },
        { 0xC5, 12 }, { 0x91, 11 }, { 0x18, 10 }, { 0x03,  5 }, { 0x30,  4 },
        { 0x22,  6 }, { 0x14,  6 }, { 0x33,  7 }, { 0xD3, 12 }, { 0xC1, 14 },
        { 0x88, 14 }, { 0xC6, 13 }, { 0x94, 11 }, { 0x77, 10 }, { 0x56,  9 },
        { 0x93, 11 }, { 0xA6, 12 }, { 0x38, 12 }, { 0x76, 10 }, { 0x72,  9 },
        { 0x31,  6 }, { 0x10,  2 }, { 0x01,  4 }, { 0x11,  5 }, { 0x15,  7 },
        { 0x08, 10 }, { 0x58, 11 }, { 0xB3, 11 }, { 0x71,  9 }, { 0x41,  8 },
        { 0x60,  7 }, { 0x17,  9 }, { 0x45,  9 }, { 0x35,  9 }, { 0xC4, 13 },
        { 0xB7, 13 }, { 0x86, 12 }, { 0x78, 11 }, { 0x64, 10 }, { 0x02,  5 },
        { 0x13,  6 }, { 0x43,  8 }, { 0x52,  8 }, { 0x90,  8 }, { 0x16,  8 },
        { 0x20,  4 },
    },
    {
        { 0x10,  2 }, { 0x04,  6 }, { 0x24,  7 }, { 0x33,  7 }, { 0x12,  5 },
        { 0x22,  5 }, { 0x14,  7 }, { 0xD3, 13 }, { 0x48, 13 }, { 0xC3, 12 },
        { 0xA1, 12 }, { 0xA2, 12 }, { 0x77, 11 }, { 0x36, 11 }, { 0x71,  9 },
        { 0x80,  8 }, { 0x13,  6 }, { 0x01,  3 }, { 0x30,  4 }, { 0x02,  4 },
        { 0x40,  5 }, { 0x70,  7 }, { 0x81, 10 }, { 0xA8, 12 }, { 0xA7, 12 },
        {    0, 11 }, { 0xA6, 11 }, { 0x08, 11 }, { 0x56, 10 }, { 0x54,  8 },
        { 0x64,  8 }, { 0x15,  8 }, { 0x97, 11 }, { 0xD8, 11 }, { 0x95, 10 },
        { 0x07, 11 }, { 0xC7, 11 }, { 0x28, 13 }, { 0x58, 13 }, { 0xC1, 13 },
        { 0xD2, 13 }, { 0x18, 11 }, { 0x62,  8 }, { 0x03,  5 }, { 0x23,  6 },
        { 0x75,  9 }, { 0x93, 10 }, { 0xC5, 11 }, { 0xB2, 12 }, { 0xD4, 12 },
        { 0x72,  9 }, { 0xC0, 10 }, { 0xA4, 11 }, { 0x91, 11 }, { 0x25,  9 },
        { 0x65,  9 }, { 0x63,  8 }, { 0x21,  5 }, { 0x50,  6 }, { 0xB4, 11 },
        { 0x68, 13 }, { 0x38, 14 }, { 0xD1, 14 }, { 0x87, 12 }, { 0x82, 10 },
        { 0x74,  9 }, { 0xD0, 10 }, { 0x06, 10 }, { 0x45,  9 }, { 0x55,  9 },
        { 0xB5, 11 }, { 0xD7, 11 }, { 0x94, 10 }, { 0x53,  8 }, { 0x31,  6 },
        { 0x41,  7 }, { 0x76, 10 }, { 0xC8, 12 }, { 0xB8, 12 }, { 0x17, 11 },
        { 0x73,  9 }, { 0x05,  8 }, { 0x43,  7 }, { 0x32,  7 }, { 0x85, 10 },
        { 0xC2, 13 }, { 0x27, 13 }, { 0xD5, 12 }, { 0x66, 11 }, { 0x90,  9 },
        { 0x16, 10 }, { 0xB6, 11 }, { 0x96, 11 }, { 0x35, 10 }, { 0xB0, 10 },
        { 0x60,  7 }, { 0x52,  8 }, { 0x34,  9 }, { 0x61,  9 }, { 0x83, 10 },
        { 0x98, 12 }, { 0x37, 13 }, { 0x78, 13 }, { 0x86, 11 }, { 0xC6, 12 },
        { 0x67, 12 }, { 0xD6, 12 }, { 0x57, 12 }, { 0x84, 10 }, { 0x44,  8 },
        { 0x42,  7 }, { 0x51,  8 }, { 0x92, 11 }, { 0xA5, 11 }, { 0xA0, 10 },
        { 0x47, 13 }, { 0x88, 13 }, { 0xB3, 12 }, { 0xC4, 13 }, { 0xB1, 13 },
        { 0x26, 12 }, { 0xB7, 12 }, { 0xA3, 12 }, { 0x46, 11 }, { 0x11,  5 },
        { 0x20,  4 },
    },
    {
        { 0xC0,  8 }, { 0x94, 10 }, { 0xA8, 10 }, { 0x67,  9 }, { 0x33,  7 },
        { 0x16,  7 }, { 0x41,  7 }, { 0x50,  5 }, { 0x11,  5 }, { 0x04,  5 },
        { 0x02,  4 }, { 0x18,  9 }, { 0x73,  9 }, { 0x55,  8 }, { 0x62,  8 },
        { 0xB2, 12 }, { 0x38, 12 }, { 0xA2, 11 }, { 0x58, 10 }, { 0x76,  9 },
        { 0x44,  7 }, { 0x64,  8 }, { 0x56,  9 }, { 0x66,  9 }, { 0x70,  6 },
        { 0x51,  8 }, { 0x47, 10 }, { 0x82, 10 }, { 0x36, 10 }, { 0xA4, 10 },
        { 0x07,  8 }, { 0x08,  9 }, { 0xD7, 10 }, { 0x77, 10 }, { 0x01,  4 },
        { 0x23,  6 }, { 0x14,  6 }, { 0x90,  7 }, { 0x32,  7 }, { 0x05,  6 },
        { 0x96,  9 }, { 0xC5, 11 }, { 0x37, 11 }, { 0x78, 10 }, { 0x84,  9 },
        { 0x26,  9 }, { 0xD1, 13 }, { 0xC1, 13 }, { 0xA1, 12 }, { 0x98, 11 },
        { 0xA7, 10 }, { 0x46,  9 }, { 0x83,  9 }, { 0xA3, 11 }, { 0x68, 11 },
        { 0x91, 10 }, { 0x24,  7 }, { 0x43,  7 }, { 0x42,  7 }, { 0x88, 11 },
        { 0xB5, 11 }, {    0, 10 }, { 0x71, 10 }, { 0xA6, 11 }, { 0x81, 11 },
        { 0xB0,  8 }, { 0x80,  7 }, { 0x95, 10 }, { 0x48, 12 }, { 0x28, 12 },
        { 0xB8, 11 }, { 0x35,  9 }, { 0x25,  8 }, { 0x40,  5 }, { 0x13,  6 },
        { 0xC8, 10 }, { 0xC4, 11 }, { 0xA5, 11 }, { 0xB7, 10 }, { 0xD8, 10 },
        { 0x52,  8 }, { 0x31,  7 }, { 0x03,  5 }, { 0x65,  8 }, { 0x63,  8 },
        { 0x06,  7 }, { 0xD3, 12 }, { 0xD4, 12 }, { 0xB1, 12 }, { 0xC2, 12 },
        { 0x87, 10 }, { 0x86,  9 }, { 0x53,  8 }, { 0x15,  7 }, { 0x22,  6 },
        { 0x60,  6 }, { 0x21,  6 }, { 0xA0,  8 }, { 0x54,  8 }, { 0xB4, 11 },
        { 0xC7, 11 }, { 0xB6, 11 }, { 0xD5, 11 }, { 0x97, 10 }, { 0xC3, 12 },
        { 0xD2, 12 }, { 0xC6, 11 }, { 0x34,  9 }, { 0x72, 10 }, { 0xD6, 11 },
        { 0xB3, 11 }, { 0x12,  6 }, { 0xD0,  9 }, { 0x61,  9 }, { 0x45,  9 },
        { 0x57, 10 }, { 0x93, 10 }, { 0x92, 11 }, { 0x27, 11 }, { 0x85, 10 },
        { 0x17,  9 }, { 0x74,  9 }, { 0x75,  9 }, { 0x30,  5 }, { 0x20,  4 },
        { 0x10,  2 },
    },
    {
        { 0x01,  3 }, { 0x05,  7 }, { 0x65,  9 }, { 0x64,  9 }, { 0x24,  8 },
        { 0xD1, 12 }, { 0x28, 13 }, { 0xA2, 13 }, { 0x83, 12 }, { 0xA5, 12 },
        { 0x76, 11 }, { 0x82, 12 }, { 0x92, 12 }, { 0xB0,  9 }, { 0x75, 10 },
        { 0x93, 11 }, { 0xA7, 14 }, { 0xD4, 14 }, { 0xB3, 13 }, { 0xB7, 12 },
        { 0x25,  9 }, { 0x41,  7 }, { 0x22,  5 }, { 0x70,  7 }, { 0x08, 11 },
        { 0x96, 11 }, { 0x16, 10 }, { 0xC5, 11 }, { 0xC6, 13 }, { 0x78, 13 },
        { 0xA4, 12 }, { 0x56, 12 }, { 0x36, 13 }, { 0xC3, 14 }, { 0x68, 14 },
        { 0x26, 12 }, { 0x67, 13 }, { 0xD7, 13 }, { 0x80,  8 }, { 0x13,  6 },
        { 0x12,  5 }, { 0x02,  4 }, { 0x32,  6 }, { 0x33,  7 }, { 0x62,  9 },
        { 0x06,  9 }, { 0xD0, 10 }, { 0x81, 11 }, { 0x95, 11 }, { 0xA0,  9 },
        { 0x40,  5 }, { 0x20,  3 }, { 0x45,  9 }, { 0x86, 12 }, { 0xD2, 13 },
        { 0xD8, 14 }, { 0x47, 15 }, { 0x88, 15 }, { 0x91, 11 }, { 0x74, 10 },
        { 0x63,  9 }, { 0x85, 11 }, { 0x77, 12 }, { 0x66, 12 }, { 0xC0, 10 },
        { 0x42,  7 }, { 0x34,  9 }, { 0x72, 10 }, { 0xD5, 13 }, { 0x38, 14 },
        { 0x57, 14 }, { 0x87, 14 }, { 0xC7, 14 }, { 0x37, 14 }, {    0, 14 },
        { 0xB5, 12 }, { 0x97, 12 }, { 0x51,  8 }, { 0x23,  7 }, { 0x21,  5 },
        { 0x30,  4 }, { 0x53,  8 }, { 0x61, 10 }, { 0x94, 12 }, { 0x58, 13 },
        { 0xA6, 13 }, { 0x07, 11 }, { 0x71, 11 }, { 0xB6, 13 }, { 0xC1, 13 },
        { 0xD6, 13 }, { 0x48, 13 }, { 0x98, 13 }, { 0xC2, 13 }, { 0xC4, 13 },
        { 0xA3, 13 }, { 0x17, 11 }, { 0x60,  7 }, { 0x04,  7 }, { 0x54,  9 },
        { 0x55, 10 }, { 0xB1, 12 }, { 0x46, 13 }, { 0xB2, 13 }, { 0x84, 12 },
        { 0xB4, 13 }, { 0xC8, 15 }, { 0xD3, 15 }, { 0xA8, 14 }, { 0x52,  8 },
        { 0x31,  6 }, { 0x50,  6 }, { 0x11,  5 }, { 0x43,  8 }, { 0x73, 10 },
        { 0x35, 11 }, { 0x18, 13 }, { 0xB8, 14 }, { 0x27, 14 }, { 0xA1, 12 },
        { 0x15,  9 }, { 0x44,  9 }, { 0x90,  9 }, { 0x14,  8 }, { 0x03,  6 },
        { 0x10,  2 },
    },
    {
        { 0x10,  2 }, { 0x60,  6 }, { 0x64,  7 }, { 0xB6, 10 }, { 0x81, 10 },
        { 0x95,  9 }, { 0xD0, 10 }, { 0x18, 10 }, { 0x71,  9 }, { 0x15,  6 },
        { 0x41,  7 }, { 0x45,  7 }, { 0x01,  4 }, { 0x30,  4 }, { 0xD7, 11 },
        { 0xB8, 11 }, { 0x92, 10 }, { 0xB5, 10 }, {    0, 11 }, { 0xC0, 11 },
        { 0x90,  8 }, { 0x35,  7 }, { 0x53,  7 }, { 0xA5, 10 }, { 0xA1, 12 },
        { 0xB2, 12 }, { 0xA3, 11 }, { 0x17,  9 }, { 0x77,  9 }, { 0xB7, 11 },
        { 0xD6, 11 }, { 0x98, 10 }, { 0x40,  5 }, { 0x24,  6 }, { 0x83,  9 },
        { 0x87, 11 }, { 0xA7, 12 }, { 0xC3, 13 }, { 0xC2, 13 }, { 0x37, 10 },
        { 0x80,  8 }, { 0x85,  9 }, { 0x88, 11 }, { 0xB3, 11 }, { 0x96, 10 },
        { 0x73,  8 }, { 0x04,  5 }, { 0x34,  7 }, { 0x65,  8 }, { 0xD8, 11 },
        { 0xA8, 11 }, { 0x27, 10 }, { 0x94,  9 }, { 0x23,  6 }, { 0x13,  6 },
        { 0x55,  7 }, { 0x33,  7 }, { 0x14,  6 }, { 0x54,  7 }, { 0x51,  8 },
        { 0x62,  8 }, { 0x22,  6 }, { 0x12,  6 }, { 0x03,  5 }, { 0x25,  7 },
        { 0x75,  8 }, { 0x76,  9 }, { 0xA6, 11 }, { 0x91, 11 }, { 0x67, 10 },
        { 0x06,  7 }, { 0x42,  7 }, { 0x32,  7 }, { 0x16,  8 }, { 0x36,  9 },
        { 0x82, 10 }, { 0x58, 10 }, { 0x21,  6 }, { 0x02,  5 }, { 0x20,  4 },
        { 0x44,  7 }, { 0x08,  8 }, { 0x56,  8 }, { 0x11,  6 }, { 0x74,  8 },
        { 0x84,  9 }, { 0xD4, 12 }, { 0xC5, 12 }, { 0xA4, 11 }, { 0x93, 10 },
        { 0x43,  7 }, { 0x78, 10 }, { 0x68, 11 }, { 0x38, 12 }, { 0xB1, 13 },
        { 0xD3, 13 }, { 0x57,  9 }, { 0x72,  9 }, { 0x46,  9 }, { 0x28, 11 },
        { 0xB4, 11 }, { 0xA2, 12 }, { 0xC8, 12 }, { 0x86, 11 }, { 0x61,  9 },
        { 0x07,  8 }, { 0x52,  8 }, { 0x66, 10 }, { 0xB0, 10 }, { 0x47, 10 },
        { 0xC6, 12 }, { 0xC7, 12 }, { 0x48, 11 }, { 0x70,  7 }, { 0x31,  7 },
        { 0xA0, 10 }, { 0x97, 11 }, { 0xD2, 14 }, { 0xD1, 15 }, { 0xC1, 15 },
        { 0xC4, 13 }, { 0xD5, 12 }, { 0x26,  9 }, { 0x63,  8 }, { 0x05,  6 },
        { 0x50,  6 },
    },
    {
        { 0x12,  5 }, { 0x61,  8 }, { 0x55,  8 }, { 0x35,  8 }, { 0x25,  8 },
        { 0x23,  7 }, { 0x64,  8 }, { 0x71,  8 }, { 0x70,  5 }, { 0xB0,  7 },
        { 0x83,  9 }, { 0x93,  9 }, { 0x86, 10 }, { 0xA3, 11 }, { 0xD1, 12 },
        {    0, 12 }, { 0x84,  9 }, { 0xA0,  7 }, { 0x41,  7 }, { 0x11,  4 },
        { 0x33,  7 }, { 0x63,  8 }, { 0x27, 10 }, { 0xB8, 12 }, { 0xD8, 12 },
        { 0x88, 13 }, { 0xA8, 13 }, { 0xB7, 12 }, { 0x76,  9 }, { 0x05,  6 },
        { 0x40,  5 }, { 0x10,  2 }, { 0x02,  5 }, { 0x80,  6 }, { 0x17,  8 },
        { 0x73,  8 }, { 0x51,  7 }, { 0x20,  4 }, { 0x90,  6 }, { 0x14,  6 },
        { 0x50,  5 }, { 0x37, 10 }, { 0x96, 10 }, { 0xA5, 11 }, { 0xA7, 12 },
        { 0xC8, 12 }, { 0x57, 10 }, { 0x36,  9 }, { 0x26,  9 }, { 0x44,  8 },
        { 0x18,  9 }, { 0xB2, 11 }, { 0x58, 11 }, { 0xA1, 11 }, { 0xD4, 12 },
        { 0xC6, 12 }, { 0x74,  8 }, { 0x56,  9 }, { 0x94,  9 }, { 0x08,  8 },
        { 0x65,  9 }, { 0x85, 10 }, { 0x28, 11 }, { 0xA2, 11 }, { 0x03,  6 },
        { 0x31,  6 }, { 0x13,  6 }, { 0x24,  8 }, { 0xC5, 12 }, { 0x48, 12 },
        { 0xA4, 11 }, { 0x82, 10 }, { 0x97, 11 }, { 0xB6, 11 }, { 0x77, 10 },
        { 0x34,  8 }, { 0xD0,  9 }, { 0x45,  9 }, { 0x32,  7 }, { 0x43,  8 },
        { 0x66, 10 }, { 0xD3, 12 }, { 0xC3, 13 }, { 0x68, 13 }, { 0x67, 11 },
        { 0xC0,  9 }, { 0x21,  6 }, { 0x01,  4 }, { 0x15,  7 }, { 0x53,  8 },
        { 0x42,  8 }, { 0x04,  6 }, { 0x06,  7 }, { 0x54,  8 }, { 0x92, 10 },
        { 0xB5, 11 }, { 0xD5, 12 }, { 0xA6, 12 }, { 0x46, 10 }, { 0xB4, 11 },
        { 0x47, 11 }, { 0x72,  9 }, { 0x62,  9 }, { 0x07,  8 }, { 0x52,  8 },
        { 0xB3, 11 }, { 0xC2, 13 }, { 0xD6, 13 }, { 0x38, 12 }, { 0x95, 10 },
        { 0x81, 10 }, { 0xB1, 11 }, { 0x87, 12 }, { 0x98, 13 }, { 0xD7, 13 },
        { 0x22,  7 }, { 0x75,  9 }, { 0x78, 11 }, { 0xC7, 13 }, { 0xD2, 13 },
        { 0xC4, 13 }, { 0xC1, 13 }, { 0x91, 10 }, { 0x16,  8 }, { 0x60,  6 },
        { 0x30,  5 },
    },
    {
        { 0x30,  4 }, { 0x31,  6 }, { 0x53,  8 }, { 0x05,  9 }, { 0x25, 12 },
        { 0xB4, 13 }, { 0x36, 13 }, { 0x94, 12 }, { 0xC6, 14 }, { 0x27, 14 },
        { 0x38, 14 }, { 0x46, 14 }, { 0x26, 12 }, { 0x65, 12 }, { 0x93, 11 },
        { 0x66, 12 }, { 0x77, 14 }, { 0x97, 14 }, { 0xA4, 14 }, { 0xB1, 14 },
        { 0xB2, 14 }, { 0xD1, 14 }, { 0xD2, 14 }, { 0xD8, 14 }, { 0x08, 12 },
        { 0x16, 12 }, { 0x18, 12 }, { 0x37, 13 }, { 0x47, 13 }, { 0x95, 12 },
        { 0x15,  9 }, { 0x42,  8 }, { 0x21,  6 }, { 0x14,  8 }, { 0x56, 11 },
        { 0x73, 11 }, { 0x06, 10 }, { 0x34,  9 }, { 0x43,  8 }, { 0x41,  9 },
        { 0x74, 11 }, { 0x86, 12 }, { 0xB7, 13 }, { 0x45, 13 }, { 0x63, 11 },
        { 0x57, 12 }, { 0x91, 12 }, { 0x01,  3 }, { 0x20,  4 }, { 0x50,  6 },
        { 0x54,  9 }, { 0x62, 10 }, { 0x55, 10 }, { 0x52,  9 }, { 0x80,  9 },
        { 0x71, 11 }, { 0x64, 11 }, { 0x82, 10 }, { 0x51,  9 }, { 0x24,  9 },
        { 0x44,  9 }, { 0x02,  5 }, { 0x40,  6 }, { 0x03,  6 }, { 0x12,  6 },
        { 0x22,  7 }, { 0x61, 10 }, { 0x76, 13 }, { 0x84, 13 }, { 0x85, 13 },
        { 0x96, 13 }, { 0x75, 12 }, { 0xB6, 13 }, { 0x58, 13 }, { 0xB0, 10 },
        { 0xD0, 11 }, { 0xA3, 13 }, { 0xD5, 13 }, { 0x81, 13 }, { 0xB3, 13 },
        { 0x04,  8 }, { 0x11,  5 }, { 0x60,  8 }, { 0x92, 10 }, { 0x72, 10 },
        { 0x33,  9 }, { 0x70,  8 }, { 0x32,  8 }, { 0x13,  7 }, { 0x90,  9 },
        { 0x83, 11 }, { 0xC0, 13 }, { 0xC4, 20 }, { 0xC5, 20 }, { 0xC7, 20 },
        { 0xC8, 20 }, { 0xD3, 20 }, { 0xD4, 20 }, { 0xD6, 20 }, { 0xD7, 20 },
        { 0x28, 21 }, { 0x48, 21 }, { 0x67, 21 }, { 0x68, 21 }, { 0x78, 21 },
        { 0x87, 21 }, { 0x88, 21 }, { 0x98, 21 }, { 0xA2, 21 }, { 0xA6, 21 },
        { 0xA8, 21 }, { 0xB5, 21 }, { 0xB8, 21 }, { 0xC1, 21 }, { 0xC2, 21 },
        { 0xC3, 21 }, {    0, 16 }, { 0xA1, 15 }, { 0xA5, 15 }, { 0xA7, 15 },
        { 0x07, 12 }, { 0x17, 12 }, { 0x35, 12 }, { 0xA0, 11 }, { 0x23,  8 },
        { 0x10,  1 },
    },
    {
        { 0x12,  5 }, { 0x75, 10 }, { 0x86, 12 }, { 0xB2, 12 }, { 0xA2, 11 },
        { 0x63, 10 }, { 0xA4, 12 }, { 0xA5, 12 }, { 0x46, 12 }, { 0x68, 14 },
        { 0xD2, 14 }, { 0x87, 13 }, { 0x71,  9 }, { 0x72,  9 }, { 0x33,  8 },
        { 0x60,  8 }, { 0x14,  8 }, { 0x42,  8 }, { 0x64, 10 }, { 0x16, 10 },
        { 0xA0, 10 }, { 0xD3, 11 }, { 0x93, 11 }, { 0x23,  8 }, { 0x30,  4 },
        { 0x11,  4 }, { 0x41,  8 }, { 0x05,  9 }, { 0x06, 10 }, { 0x65, 11 },
        { 0x66, 12 }, { 0x08, 13 }, { 0x18, 14 }, { 0xB6, 14 }, { 0x70,  8 },
        { 0x53,  8 }, { 0x61, 10 }, { 0x82, 11 }, { 0xC4, 13 }, { 0xB7, 14 },
        { 0xC1, 19 }, { 0xC2, 19 }, { 0x27, 20 }, { 0x37, 20 }, { 0x38, 20 },
        { 0x48, 20 }, { 0x57, 20 }, { 0x58, 20 }, { 0x78, 20 }, { 0x88, 20 },
        { 0x98, 20 }, { 0xA7, 20 }, { 0xA8, 20 }, { 0xB8, 20 }, { 0x28, 16 },
        { 0x26, 15 }, { 0xB4, 12 }, { 0x15,  9 }, { 0x52,  8 }, { 0x22,  7 },
        { 0x55, 11 }, { 0xD5, 12 }, { 0xD1, 13 }, { 0x17, 13 }, { 0x34, 10 },
        { 0x73,  9 }, { 0x51,  8 }, { 0x40,  7 }, { 0x03,  6 }, { 0x20,  3 },
        { 0x13,  6 }, { 0x45, 11 }, { 0x95, 12 }, { 0x96, 13 }, { 0x76, 13 },
        { 0x84, 11 }, { 0x97, 13 }, { 0xA6, 13 }, { 0x25, 12 }, { 0x94, 12 },
        { 0x91, 12 }, { 0x90, 11 }, { 0xB3, 11 }, { 0xA3, 11 }, { 0xC0, 11 },
        { 0xB5, 13 }, { 0x67, 15 }, { 0xC5, 15 }, { 0xC6, 15 }, { 0x47, 16 },
        { 0xC7, 16 }, { 0x81, 12 }, { 0x83, 11 }, { 0x92, 11 }, { 0x43,  9 },
        { 0x77, 13 }, { 0xD4, 13 }, { 0x35, 12 }, { 0xD0, 11 }, { 0x44, 10 },
        { 0x24, 10 }, { 0xC3, 13 }, { 0x07, 13 }, { 0x56, 13 }, { 0xC8, 16 },
        { 0xD7, 16 }, { 0xD8, 16 }, {    0, 16 }, { 0xA1, 14 }, { 0xB0, 11 },
        { 0x80, 11 }, { 0x36, 13 }, { 0xB1, 13 }, { 0xD6, 13 }, { 0x85, 13 },
        { 0x74, 10 }, { 0x62, 10 }, { 0x54, 10 }, { 0x21,  5 }, { 0x02,  5 },
        { 0x31,  6 }, { 0x50,  7 }, { 0x04,  8 }, { 0x32,  8 }, { 0x10,  2 },
        { 0x01,  2 },
    },
};

const uint8_t ff_wmv3_dc_scale_table[32] = {
     0,  2,  4,  8,  8,  8,  9,  9, 10, 10, 11, 11, 12, 12, 13, 13,
    14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21
};

/* P-Picture CBPCY VLC tables */
// Looks like original tables are not conforming to standard at all. Are they used for old WMV?
const uint16_t ff_vc1_cbpcy_p_codes[4][64] = {
  {
      0,   6,  15,  13,  13,  11,   3,  13,   5,   8,  49,  10,  12, 114, 102, 119,
      1,  54,  96,   8,  10, 111,   5,  15,  12,  10,   2,  12,  13, 115,  53,  63,
      1,   7,   1,   7,  14,  12,   4,  14,   1,   9,  97,  11,   7,  58,  52,  62,
      4, 103,   1,   9,  11,  56, 101, 118,   4, 110, 100,  30,   2,   5,   4,   3
  },
  {
      0,   9,   1,  18,   5,  14, 237,  26,   3, 121,   3,  22,  13,  16,   6,  30,
      2,  10,   1,  20,  12, 241,   5,  28,  16,  12,   3,  24,  28, 124, 239, 247,
      1, 240,   1,  19,  18,  15,   4,  27,   1, 122,   2,  23,   1,  17,   7,  31,
      1,  11,   2,  21,  19, 246, 238,  29,  17,  13, 236,  25,  58,  63,   8, 125
  },
  {
      0, 201,  25, 231,   5, 221,   1,   3,   2, 414,   2, 241,  16, 225, 195, 492,
      2, 412,   1, 240,   7, 224,  98, 245,   1, 220,  96,   5,   9, 230, 101, 247,
      1, 102,   1, 415,  24,   3,   2, 244,   3,  54,   3, 484,  17, 114, 200, 493,
      3, 413,   1,   4,  13, 113,  99, 485,   4, 111, 194, 243,   5,  29,  26,  31
  },
  {
      0,  28,  12,  44,   3,  36,  20,  52,   2,  32,  16,  48,   8,  40,  24,  28,
      1,  30,  14,  46,   6,  38,  22,  54,   3,  34,  18,  50,  10,  42,  26,  30,
      1,  29,  13,  45,   5,  37,  21,  53,   2,  33,  17,  49,   9,  41,  25,  29,
      1,  31,  15,  47,   7,  39,  23,  55,   4,  35,  19,  51,  11,  43,  27,  31
   }
};

const uint8_t ff_vc1_cbpcy_p_bits[4][64] = {
  {
    13,  13,   7,  13,   7,  13,  13,  12,   6,  13,   7,  12,   6,   8,   8,   8,
     5,   7,   8,  12,   6,   8,  13,  12,   7,  13,  13,  12,   6,   8,   7,   7,
     6,  13,   8,  12,   7,  13,  13,  12,   7,  13,   8,  12,   5,   7,   7,   7,
     6,   8,  13,  12,   6,   7,   8,   8,   5,   8,   8,   6,   3,   3,   3,   2
  },
  {
    14,  13,   8,  13,   3,  13,   8,  13,   3,   7,   8,  13,   4,  13,  13,  13,
     3,  13,  13,  13,   4,   8,  13,  13,   5,  13,  13,  13,   5,   7,   8,   8,
     3,   8,  14,  13,   5,  13,  13,  13,   4,   7,  13,  13,   6,  13,  13,  13,
     5,  13,   8,  13,   5,   8,   8,  13,   5,  13,   8,  13,   6,   6,  13,   7
  },
  {
    13,   8,   6,   8,   4,   8,  13,  12,   4,   9,   8,   8,   5,   8,   8,   9,
     5,   9,  10,   8,   4,   8,   7,   8,   6,   8,   7,  13,   4,   8,   7,   8,
     5,   7,   8,   9,   6,  13,  13,   8,   4,   6,   8,   9,   5,   7,   8,   9,
     5,   9,   9,  13,   5,   7,   7,   9,   4,   7,   8,   8,   3,   5,   5,   5
  },
  {
     9,   9,   9,   9,   2,   9,   9,   9,   2,   9,   9,   9,   9,   9,   9,   8,
     3,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   8,
     2,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   8,
     9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   9,   8
  }
};

/* Interlaced CBPCY VLC tables (Table 124 - Table 131) */
const uint16_t ff_vc1_icbpcy_p_codes[8][63] = {
  {
    0x2F1A, 0x2F1B, 0x178C, 0x0090, 0x02A8, 0x02A9, 0x0BC7, 0x0091,
    0x02AA, 0x02AB, 0x05E0, 0x004A, 0x0096, 0x0097, 0x00BD, 0x0092,
    0x02AC, 0x02AD, 0x05E1, 0x0098, 0x0132, 0x0133, 0x0179, 0x0134,
    0x026A, 0x026B, 0x02FC, 0x004E, 0x0040, 0x0041, 0x002B, 0x0093,
    0x02AE, 0x02AF, 0x05E2, 0x0136, 0x026E, 0x026F, 0x02FD, 0x009E,
    0x013E, 0x013F, 0x017F, 0x0050, 0x0042, 0x0043, 0x002C, 0x0051,
    0x00A4, 0x00A5, 0x00BE, 0x0053, 0x0044, 0x0045, 0x002D, 0x0054,
    0x0046, 0x0047, 0x002E, 0x0003, 0x0000, 0x0001, 0x0001
  },
  {
    0x0041, 0x0042, 0x0100, 0x0043, 0x0088, 0x0089, 0x0101, 0x0045,
    0x008C, 0x008D, 0x0102, 0x0010, 0x0022, 0x0023, 0x0024, 0x0047,
    0x0010, 0x0011, 0x0103, 0x0025, 0x0058, 0x0059, 0x005A, 0x005B,
    0x005A, 0x005B, 0x005C, 0x000C, 0x0030, 0x0031, 0x0019, 0x0009,
    0x0014, 0x0015, 0x002C, 0x005C, 0x005D, 0x005E, 0x005F, 0x0026,
    0x005D, 0x005E, 0x005F, 0x000D, 0x0034, 0x0035, 0x001B, 0x0014,
    0x0027, 0x002A, 0x002B, 0x000E, 0x0038, 0x0039, 0x001D, 0x000F,
    0x003C, 0x003D, 0x001F, 0x0005, 0x0009, 0x0000, 0x0003
  },
  {
    0x0032, 0x0033, 0x001A, 0x0026, 0x00E4, 0x00E5, 0x01E6, 0x0027,
    0x00E6, 0x00E7, 0x01E7, 0x000E, 0x0063, 0x006C, 0x0077, 0x0028,
    0x00E8, 0x00E9, 0x01E8, 0x007B, 0x00DA, 0x00DB, 0x00EC, 0x00F5,
    0x01B8, 0x01B9, 0x01DA, 0x0021, 0x004B, 0x0054, 0x002B, 0x0029,
    0x00EA, 0x00EB, 0x01E9, 0x004A, 0x01BA, 0x01BB, 0x01DB, 0x0020,
    0x00DE, 0x00DF, 0x00F2, 0x0022, 0x0055, 0x0058, 0x002D, 0x000F,
    0x0070, 0x0071, 0x0078, 0x0023, 0x0059, 0x005C, 0x002F, 0x0024,
    0x005D, 0x0062, 0x0030, 0x0002, 0x001F, 0x0006, 0x0000
  },
  {
    0x0028, 0x0029, 0x009D, 0x0000, 0x01EA, 0x01EB, 0x01EC, 0x0001,
    0x01ED, 0x01EE, 0x01EF, 0x0005, 0x00F0, 0x00F1, 0x003B, 0x0002,
    0x01F0, 0x01F1, 0x01F2, 0x003F, 0x015C, 0x015D, 0x0099, 0x0010,
    0x03D0, 0x03D1, 0x0130, 0x000F, 0x009E, 0x009F, 0x00FB, 0x0003,
    0x01F3, 0x01F4, 0x01F5, 0x0011, 0x03D2, 0x03D3, 0x0131, 0x0009,
    0x015E, 0x015F, 0x009C, 0x0010, 0x00A8, 0x00A9, 0x0038, 0x0006,
    0x00F2, 0x00F3, 0x004D, 0x0011, 0x00AA, 0x00AB, 0x0039, 0x0012,
    0x00AC, 0x00AD, 0x003A, 0x0006, 0x0016, 0x0017, 0x000E
  },
  {
    0x003C, 0x003D, 0x001F, 0x000A, 0x0061, 0x0062, 0x0002, 0x000B,
    0x0063, 0x0064, 0x0003, 0x0007, 0x0003, 0x0004, 0x000B, 0x000C,
    0x0065, 0x0066, 0x0004, 0x0012, 0x000A, 0x000B, 0x0014, 0x001B,
    0x0018, 0x0019, 0x0034, 0x002C, 0x0067, 0x0068, 0x0035, 0x000D,
    0x0069, 0x006C, 0x0005, 0x0060, 0x001A, 0x001B, 0x0035, 0x0013,
    0x000E, 0x000F, 0x0015, 0x002D, 0x006D, 0x006E, 0x0038, 0x0008,
    0x0008, 0x0009, 0x000C, 0x002E, 0x006F, 0x0072, 0x003A, 0x002F,
    0x0073, 0x0000, 0x003B, 0x0007, 0x0014, 0x0015, 0x0004
  },
  {
    0x0038, 0x0039, 0x009D, 0x000A, 0x0091, 0x0092, 0x0093, 0x000B,
    0x0094, 0x0095, 0x0096, 0x0003, 0x00EE, 0x00EF, 0x0036, 0x000C,
    0x0097, 0x0098, 0x0099, 0x0008, 0x01E4, 0x01E5, 0x006A, 0x0018,
    0x03CC, 0x03CD, 0x00D6, 0x000E, 0x009E, 0x009F, 0x00F5, 0x000D,
    0x009A, 0x009B, 0x009C, 0x0019, 0x03CE, 0x03CF, 0x00D7, 0x0009,
    0x01E8, 0x01E9, 0x0090, 0x000F, 0x00E8, 0x00E9, 0x00F6, 0x0005,
    0x00F0, 0x00F1, 0x0037, 0x0010, 0x00EA, 0x00EB, 0x00F7, 0x0011,
    0x00EC, 0x00ED, 0x0034, 0x0000, 0x003E, 0x003F, 0x0002
  },
  {
    0x003C, 0x003D, 0x01CF, 0x0000, 0x00BF, 0x00E0, 0x01FC, 0x0001,
    0x00E1, 0x00E2, 0x01FD, 0x0009, 0x01F1, 0x01F2, 0x01F3, 0x0002,
    0x00E3, 0x00E4, 0x01FE, 0x0011, 0x03EE, 0x03EF, 0x03F0, 0x0021,
    0x07E2, 0x07E3, 0x07E4, 0x0018, 0x03F7, 0x03FE, 0x03FF, 0x0003,
    0x00E5, 0x00E6, 0x0080, 0x002E, 0x07E5, 0x07E6, 0x07E7, 0x0016,
    0x03F4, 0x03F5, 0x03F6, 0x0019, 0x0102, 0x0103, 0x0104, 0x000A,
    0x01F4, 0x01F5, 0x01F6, 0x001A, 0x0105, 0x0106, 0x0107, 0x001B,
    0x0178, 0x0179, 0x01CE, 0x001D, 0x00BD, 0x00BE, 0x01F0
  },
  {
    0x0003, 0x0004, 0x01B6, 0x0004, 0x002E, 0x002F, 0x000E, 0x0005,
    0x0030, 0x0031, 0x000F, 0x0003, 0x000A, 0x000B, 0x0014, 0x0006,
    0x0032, 0x0033, 0x0010, 0x0005, 0x0030, 0x0031, 0x0032, 0x0009,
    0x0066, 0x0067, 0x0068, 0x001D, 0x01B7, 0x01B8, 0x01B9, 0x0007,
    0x0034, 0x0035, 0x0011, 0x0016, 0x0069, 0x006A, 0x006B, 0x000A,
    0x0036, 0x0037, 0x00D8, 0x001E, 0x01BA, 0x01BB, 0x01BC, 0x0004,
    0x0015, 0x0016, 0x0017, 0x001F, 0x01BD, 0x01BE, 0x01BF, 0x0000,
    0x0010, 0x0011, 0x0012, 0x001C, 0x00D9, 0x00DA, 0x0013
  }
};

const uint8_t ff_vc1_icbpcy_p_bits[8][63] = {
  {
    15, 15, 14,  9, 11, 11, 13,  9, 11, 11, 12,  8,  9,  9,  9,  9,
    11, 11, 12,  9, 10, 10, 10, 10, 11, 11, 11,  8,  8,  8,  7,  9,
    11, 11, 12, 10, 11, 11, 11,  9, 10, 10, 10,  8,  8,  8,  7,  8,
     9,  9,  9,  8,  8,  8,  7,  8,  8,  8,  7,  3,  3,  3,  1
  },
  {
     7,  7,  9,  7,  8,  8,  9,  7,  8,  8,  9,  6,  7,  7,  7,  7,
     7,  7,  9,  7,  8,  8,  8,  8,  9,  9,  9,  6,  7,  7,  6,  6,
     7,  7,  8,  8,  9,  9,  9,  7,  8,  8,  8,  6,  7,  7,  6,  6,
     7,  7,  7,  6,  7,  7,  6,  6,  7,  7,  6,  3,  4,  3,  2
  },
  {
     6,  6,  5,  6,  8,  8,  9,  6,  8,  8,  9,  5,  7,  7,  7,  6,
     8,  8,  9,  7,  8,  8,  8,  8,  9,  9,  9,  6,  7,  7,  6,  6,
     8,  8,  9,  7,  9,  9,  9,  6,  8,  8,  8,  6,  7,  7,  6,  5,
     7,  7,  7,  6,  7,  7,  6,  6,  7,  7,  6,  3,  5,  4,  2
  },
  {
     6,  6,  8,  4,  9,  9,  9,  4,  9,  9,  9,  4,  8,  8,  7,  4,
     9,  9,  9,  6,  9,  9,  8,  6, 10, 10,  9,  5,  8,  8,  8,  4,
     9,  9,  9,  6, 10, 10,  9,  5,  9,  9,  8,  5,  8,  8,  7,  4,
     8,  8,  7,  5,  8,  8,  7,  5,  8,  8,  7,  3,  5,  5,  4
  },
  {
     6,  6,  5,  5,  7,  7,  7,  5,  7,  7,  7,  5,  6,  6,  6,  5,
     7,  7,  7,  6,  7,  7,  7,  7,  8,  8,  8,  6,  7,  7,  6,  5,
     7,  7,  7,  7,  8,  8,  8,  6,  7,  7,  7,  6,  7,  7,  6,  5,
     6,  6,  6,  6,  7,  7,  6,  6,  7,  6,  6,  4,  5,  5,  3
  },
  {
     6,  6,  8,  4,  8,  8,  8,  4,  8,  8,  8,  4,  8,  8,  7,  4,
     8,  8,  8,  5,  9,  9,  8,  6, 10, 10,  9,  5,  8,  8,  8,  4,
     8,  8,  8,  6, 10, 10,  9,  5,  9,  9,  8,  5,  8,  8,  8,  4,
     8,  8,  7,  5,  8,  8,  8,  5,  8,  8,  7,  3,  6,  6,  4
  },
  {
     6,  6,  9,  3,  8,  8,  9,  3,  8,  8,  9,  4,  9,  9,  9,  3,
     8,  8,  9,  5, 10, 10, 10,  6, 11, 11, 11,  5, 10, 10, 10,  3,
     8,  8,  8,  6, 11, 11, 11,  5, 10, 10, 10,  5,  9,  9,  9,  4,
     9,  9,  9,  5,  9,  9,  9,  5,  9,  9,  9,  5,  8,  8,  9
  },
  {
     6,  6, 10,  3,  7,  7,  7,  3,  7,  7,  7,  4,  8,  8,  8,  3,
     7,  7,  7,  5,  9,  9,  9,  6, 10, 10, 10,  6, 10, 10, 10,  3,
     7,  7,  7,  6, 10, 10, 10,  5,  9,  9,  9,  6, 10, 10, 10,  4,
     8,  8,  8,  6, 10, 10, 10,  5,  9,  9,  9,  6,  9,  9,  9
  }
};

/* MacroBlock Transform Type: 7.1.3.11, p89
 * 8x8:B
 * 8x4:B:btm  8x4:B:top  8x4:B:both,
 * 4x8:B:right  4x8:B:left  4x8:B:both
 * 4x4:B  8x8:MB
 * 8x4:MB:btm  8x4:MB:top  8x4,MB,both
 * 4x8,MB,right  4x8,MB,left
 * 4x4,MB                               */
const uint16_t ff_vc1_ttmb_codes[3][16] = {
  {
    0x0003,
    0x002E, 0x005F, 0x0000,
    0x0016, 0x0015, 0x0001,
    0x0004, 0x0014,
    0x02F1, 0x0179, 0x017B,
    0x0BC0, 0x0BC1, 0x05E1,
    0x017A
  },
  {
    0x0006,
    0x0006, 0x0003, 0x0007,
    0x000F, 0x000E, 0x0000,
    0x0002, 0x0002,
    0x0014, 0x0011, 0x000B,
    0x0009, 0x0021, 0x0015,
    0x0020
  },
  {
    0x0006,
    0x0000, 0x000E, 0x0005,
    0x0002, 0x0003, 0x0003,
    0x000F, 0x0002,
    0x0081, 0x0021, 0x0009,
    0x0101, 0x0041, 0x0011,
    0x0100
  }
};

const uint8_t ff_vc1_ttmb_bits[3][16] = {
  {
     2,
     6,  7,  2,
     5,  5,  2,
     3,  5,
    10,  9,  9,
    12, 12, 11,
     9
  },
  {
    3,
    4, 4, 4,
    4, 4, 3,
    3, 2,
    7, 7, 6,
    6, 8, 7,
    8
  },
  {
     3,
     3, 4, 5,
     3, 3, 4,
     4, 2,
    10, 8, 6,
    11, 9, 7,
    11
  }
};
/* TTBLK (Transform Type per Block) tables */
const uint8_t ff_vc1_ttblk_tabs[3][8][2] = {
    {
        { TT_8X4, 2 }, { TT_4X8, 2 }, { TT_8X4_TOP, 5 }, { TT_8X4_BOTTOM, 5 },
        { TT_4X8_RIGHT, 5 }, { TT_4X8_LEFT, 5 }, { TT_4X4, 3 }, { TT_8X8, 2 },
    },
    {
        { TT_4X8_RIGHT, 3 }, { TT_4X8_LEFT, 3 }, { TT_4X4, 3 }, { TT_8X4, 3 },
        { TT_8X4_BOTTOM, 4 }, { TT_8X4_TOP, 4 }, { TT_4X8, 3 }, { TT_8X8, 2 },
    },
    {
        { TT_4X8, 3 }, { TT_4X4, 3 }, { TT_8X8, 2 }, { TT_8X4_BOTTOM, 3 },
        { TT_8X4, 4 }, { TT_8X4_TOP, 4 }, { TT_4X8_RIGHT, 3 }, { TT_4X8_LEFT, 3 },
    },
};

/* SUBBLKPAT tables, p93-94, reordered and offset by 1 */
const uint8_t ff_vc1_subblkpat_tabs[3][15][2] = {
    {
        { 0x0B, 4 }, { 0x0D, 4 }, { 0x07, 4 }, { 0x0C, 5 }, { 0x03, 5 },
        { 0x0A, 5 }, { 0x05, 5 }, { 0x08, 5 }, { 0x04, 5 }, { 0x02, 5 },
        { 0x06, 6 }, { 0x09, 6 }, { 0x01, 5 }, { 0x0E, 5 }, { 0x0F, 1 },
    },
    {
        { 0x02, 3 }, { 0x06, 5 }, { 0x09, 5 }, { 0x0C, 4 }, { 0x0F, 2 },
        { 0x03, 4 }, { 0x0A, 4 }, { 0x05, 4 }, { 0x0E, 5 }, { 0x07, 5 },
        { 0x0D, 5 }, { 0x0B, 5 }, { 0x08, 4 }, { 0x01, 4 }, { 0x04, 4 },
    },
    {
        { 0x06, 5 }, { 0x09, 5 }, { 0x0C, 4 }, { 0x03, 4 }, { 0x0A, 4 },
        { 0x04, 3 }, { 0x08, 3 }, { 0x05, 4 }, { 0x0E, 5 }, { 0x0D, 5 },
        { 0x01, 3 }, { 0x02, 3 }, { 0x07, 5 }, { 0x0B, 5 }, { 0x0F, 4 },
    }
};

/* MV differential tables, p265 */
const uint8_t ff_vc1_mv_diff_tabs[4][73][2] = {
    {
        { 0x08,  6 }, { 0x0C,  7 }, { 0x10,  7 }, { 0x14,  8 }, { 0x18, 14 },
        { 0x34, 14 }, { 0x38, 14 }, { 0x74, 14 }, { 0x78, 14 }, { 0xA4, 14 },
        { 0xA8, 14 }, { 0xAC, 14 }, { 0xB0, 13 }, { 0xB4, 13 }, { 0x00, 13 },
        { 0x19, 13 }, { 0x58, 10 }, { 0x54,  9 }, { 0x30,  7 }, { 0x24,  6 },
        { 0x28,  5 }, { 0x2C,  6 }, { 0x44,  6 }, { 0x48,  6 }, { 0x4C,  6 },
        { 0x50,  8 }, { 0x64,  9 }, { 0x84, 10 }, { 0x8C, 10 }, { 0x68,  7 },
        { 0x6C,  7 }, { 0x70,  7 }, { 0x88,  9 }, { 0x94, 10 }, { 0x98, 10 },
        { 0x90,  8 }, { 0x11,  8 }, { 0x15,  9 }, { 0x39,  9 }, { 0x06,  6 },
        { 0x09,  5 }, { 0x05,  3 }, { 0x0D,  6 }, { 0x35,  7 }, { 0x59,  9 },
        { 0x79, 10 }, { 0xA5, 10 }, { 0x65,  8 }, { 0x25,  5 }, { 0x29,  4 },
        { 0x2D,  4 }, { 0x31,  5 }, { 0x45,  6 }, { 0x55,  6 }, { 0x49,  5 },
        { 0x4D,  5 }, { 0x51,  5 }, { 0x69,  5 }, { 0x6D,  5 }, { 0x71,  5 },
        { 0x75,  7 }, { 0x85,  7 }, { 0x89,  7 }, { 0x8D,  7 }, { 0x91,  7 },
        { 0x95,  7 }, { 0x99,  8 }, { 0xA9,  9 }, { 0xB1,  9 }, { 0xAD,  8 },
        { 0xB5,  9 }, { 0x01,  9 }, { 0x07,  3 }, /* 73 elements */
    },
    {
        { 0x08,  5 }, { 0x0C,  7 }, { 0x10,  7 }, { 0x14,  6 }, { 0x18,  6 },
        { 0x2C,  6 }, { 0x24,  5 }, { 0x28,  5 }, { 0x30,  7 }, { 0x38,  7 },
        { 0x44,  8 }, { 0x48,  8 }, { 0x4C,  8 }, { 0x50, 14 }, { 0x6C, 14 },
        { 0x70, 14 }, { 0x84, 14 }, { 0x8C, 14 }, { 0x90, 14 }, { 0x94, 14 },
        { 0x98, 14 }, { 0xA4, 14 }, { 0xA8, 14 }, { 0xAC, 14 }, { 0xB0, 14 },
        { 0xB4, 14 }, { 0x00, 14 }, { 0x06, 14 }, { 0x65, 14 }, { 0x85, 13 },
        { 0x95, 13 }, { 0xA5, 13 }, { 0xA9, 13 }, { 0xAD, 13 }, { 0xB1, 13 },
        { 0xB5, 13 }, { 0x01, 13 }, { 0x54,  9 }, { 0x34,  5 }, { 0x58,  9 },
        { 0x64,  9 }, { 0x68,  8 }, { 0x74,  9 }, { 0x78,  9 }, { 0x88,  9 },
        { 0x39,  9 }, { 0x0D,  6 }, { 0x05,  2 }, { 0x09,  3 }, { 0x11,  8 },
        { 0x15,  8 }, { 0x31,  8 }, { 0x51,  8 }, { 0x19,  6 }, { 0x2D,  6 },
        { 0x35,  6 }, { 0x29,  4 }, { 0x25,  3 }, { 0x45,  6 }, { 0x49,  6 },
        { 0x4D,  6 }, { 0x55,  8 }, { 0x59,  8 }, { 0x69,  7 }, { 0x6D,  7 },
        { 0x71,  8 }, { 0x79,  9 }, { 0x89,  9 }, { 0x75,  7 }, { 0x8D,  8 },
        { 0x91,  9 }, { 0x99,  9 }, { 0x07,  5 }, /* 73 elements */
    },
    {
        { 0x08,  3 }, { 0x0C, 12 }, { 0x10, 12 }, { 0x14, 12 }, { 0x18, 12 },
        { 0x2C, 11 }, { 0x30, 11 }, { 0x34, 11 }, { 0x38, 11 }, { 0x44, 11 },
        { 0x48, 11 }, { 0x4C, 11 }, { 0x50, 11 }, { 0x54, 11 }, { 0x58, 11 },
        { 0x64, 11 }, { 0x68, 11 }, { 0x6C, 11 }, { 0x70, 11 }, { 0x74, 11 },
        { 0x78, 11 }, { 0x84, 11 }, { 0x88, 11 }, { 0x8C, 11 }, { 0x90, 11 },
        { 0x94, 11 }, { 0x98, 11 }, { 0xA4, 11 }, { 0xA8, 11 }, { 0xAC, 11 },
        { 0xB0, 11 }, { 0xB4, 11 }, { 0x00, 11 }, { 0x06, 11 }, { 0x0D, 11 },
        { 0x11, 11 }, { 0x15, 11 }, { 0x19, 11 }, { 0x2D, 11 }, { 0x31, 11 },
        { 0x35, 11 }, { 0x39, 11 }, { 0x45, 11 }, { 0x49, 11 }, { 0x4D, 11 },
        { 0x51, 11 }, { 0x55, 11 }, { 0x59, 11 }, { 0x65, 11 }, { 0x69, 11 },
        { 0x6D, 11 }, { 0x71, 11 }, { 0x75, 11 }, { 0x79, 11 }, { 0x85, 11 },
        { 0x89, 11 }, { 0x8D, 11 }, { 0x91, 11 }, { 0x95, 11 }, { 0x99, 11 },
        { 0xA5, 11 }, { 0xA9, 11 }, { 0xAD, 11 }, { 0xB1, 11 }, { 0xB5, 11 },
        { 0x01, 11 }, { 0x07, 11 }, { 0x09,  5 }, { 0x28,  4 }, { 0x24,  3 },
        { 0x25,  4 }, { 0x29,  4 }, { 0x05,  1 }, /* 73 elements */
    },
    {
        { 0x08, 15 }, { 0x10, 15 }, { 0x14, 15 }, { 0x18, 15 }, { 0x24, 15 },
        { 0x2C, 15 }, { 0x48, 15 }, { 0x64, 15 }, { 0x28, 12 }, { 0x0C, 11 },
        { 0x30, 12 }, { 0x38, 12 }, { 0x34, 11 }, { 0x44, 12 }, { 0x4C, 12 },
        { 0x50, 12 }, { 0x54, 12 }, { 0x58, 12 }, { 0x68, 15 }, { 0x70, 15 },
        { 0x98, 15 }, { 0xA4, 15 }, { 0xA8, 15 }, { 0xB0, 15 }, { 0x00, 14 },
        { 0x6C, 12 }, { 0x84, 12 }, { 0x74, 10 }, { 0x78, 11 }, { 0x88, 11 },
        { 0x8C, 10 }, { 0x90, 11 }, { 0xAC, 11 }, { 0x94, 10 }, { 0xB4, 10 },
        { 0x06, 10 }, { 0xA5, 10 }, { 0x15,  8 }, { 0x11,  7 }, { 0x19,  9 },
        { 0x85,  9 }, { 0x39,  8 }, { 0x0D,  5 }, { 0x05,  4 }, { 0x09,  4 },
        { 0x25,  5 }, { 0x31,  5 }, { 0x29,  3 }, { 0x2D,  4 }, { 0x35,  6 },
        { 0x55,  6 }, { 0x45,  5 }, { 0x49,  4 }, { 0x51,  5 }, { 0x59,  8 },
        { 0xA9,  8 }, { 0x65,  7 }, { 0x75,  6 }, { 0x4D,  3 }, { 0x69,  5 },
        { 0x6D,  5 }, { 0x71,  5 }, { 0x79,  7 }, { 0x89,  7 }, { 0x8D,  6 },
        { 0x91,  6 }, { 0x95,  6 }, { 0x99,  7 }, { 0xAD,  8 }, { 0xB1,  8 },
        { 0xB5,  7 }, { 0x01,  7 }, { 0x07,  4 }, /* 73 elements */
    },
};

/* DC differentials low+hi-mo, p217 are the same as in msmpeg4data .h */

/* Table 232 */
const uint8_t ff_vc1_simple_progressive_4x4_zz [16] = {
     0,     8,    16,     1,
     9,    24,    17,     2,
    10,    18,    25,     3,
    11,    26,    19,    27
};

const uint8_t ff_vc1_adv_progressive_8x4_zz [32] = { /* Table 233 */
     0,     8,     1,    16,     2,     9,    10,     3,
    24,    17,     4,    11,    18,    12,     5,    19,
    25,    13,    20,    26,    27,     6,    21,    28,
    14,    22,    29,     7,    30,    15,    23,    31
};

const uint8_t ff_vc1_adv_progressive_4x8_zz [32] = { /* Table 234 */
     0,     1,     8,     2,
     9,    16,    17,    24,
    10,    32,    25,    18,
    40,     3,    33,    26,
    48,    11,    56,    41,
    34,    49,    57,    42,
    19,    50,    27,    58,
    35,    43,    51,    59
};

const uint8_t ff_vc1_adv_interlaced_8x8_zz [64] = { /* Table 235 */
     0,     8,     1,    16,    24,     9,     2,    32,
    40,    48,    56,    17,    10,     3,    25,    18,
    11,     4,    33,    41,    49,    57,    26,    34,
    42,    50,    58,    19,    12,     5,    27,    20,
    13,     6,    35,    28,    21,    14,     7,    15,
    22,    29,    36,    43,    51,    59,    60,    52,
    44,    37,    30,    23,    31,    38,    45,    53,
    61,    62,    54,    46,    39,    47,    55,    63
};

const uint8_t ff_vc1_adv_interlaced_8x4_zz [32] = { /* Table 236 */
     0,     8,    16,    24,     1,     9,     2,    17,
    25,    10,     3,    18,    26,     4,    11,    19,
    12,     5,    13,    20,    27,     6,    21,    28,
    14,    22,    29,     7,    30,    15,    23,    31
};

const uint8_t ff_vc1_adv_interlaced_4x8_zz [32] = { /* Table 237 */
     0,     1,     2,     8,
    16,     9,    24,    17,
    10,     3,    32,    40,
    48,    56,    25,    18,
    33,    26,    41,    34,
    49,    57,    11,    42,
    19,    50,    27,    58,
    35,    43,    51,    59
};

const uint8_t ff_vc1_adv_interlaced_4x4_zz [16] = { /* Table 238 */
     0,     8,    16,    24,
     1,     9,    17,     2,
    25,    10,    18,     3,
    26,    11,    19,    27
};


/* DQScale as specified in 8.1.3.9 - almost identical to 0x40000/i */
const int32_t ff_vc1_dqscale[63] = {
    0x40000, 0x20000, 0x15555, 0x10000, 0xCCCD, 0xAAAB, 0x9249, 0x8000,
     0x71C7,  0x6666,  0x5D17,  0x5555, 0x4EC5, 0x4925, 0x4444, 0x4000,
     0x3C3C,  0x38E4,  0x35E5,  0x3333, 0x30C3, 0x2E8C, 0x2C86, 0x2AAB,
     0x28F6,  0x2762,  0x25ED,  0x2492, 0x234F, 0x2222, 0x2108, 0x2000,
     0x1F08,  0x1E1E,  0x1D42,  0x1C72, 0x1BAD, 0x1AF3, 0x1A42, 0x199A,
     0x18FA,  0x1862,  0x17D0,  0x1746, 0x16C1, 0x1643, 0x15CA, 0x1555,
     0x14E6,  0x147B,  0x1414,  0x13B1, 0x1352, 0x12F7, 0x129E, 0x1249,
     0x11F7,  0x11A8,  0x115B,  0x1111, 0x10C9, 0x1084, 0x1041
};

/* P Interlaced field picture MV predictor scaling values (Table 114) */
const uint16_t ff_vc1_field_mvpred_scales[2][7][4] = {
// Refdist:
//      0       1       2       3 or greater
  { // current field is first
    { 128,    192,    213,    224 },   // SCALEOPP
    { 512,    341,    307,    293 },   // SCALESAME1
    { 219,    236,    242,    245 },   // SCALESAME2
    {  32,     48,     53,     56 },   // SCALEZONE1_X
    {   8,     12,     13,     14 },   // SCALEZONE1_Y
    {  37,     20,     14,     11 },   // ZONE1OFFSET_X
    {  10,      5,      4,      3 }    // ZONE1OFFSET_Y
  },
  { // current field is second
    { 128,     64,     43,     32 },   // SCALEOPP
    { 512,   1024,   1536,   2048 },   // SCALESAME1
    { 219,    204,    200,    198 },   // SCALESAME2
    {  32,     16,     11,      8 },   // SCALEZONE1_X
    {   8,      4,      3,      2 },   // SCALEZONE1_Y
    {  37,     52,     56,     58 },   // ZONE1OFFSET_X
    {  10,     13,     14,     15 }    // ZONE1OFFSET_Y
  }
};

/* B Interlaced field picture backward MV predictor scaling values for first field (Table 115) */
const uint16_t ff_vc1_b_field_mvpred_scales[7][4] = {
    // BRFD:
    //  0       1       2       3 or greater
    { 171,    205,    219,    228 },   // SCALESAME
    { 384,    320,    299,    288 },   // SCALEOPP1
    { 230,    239,    244,    246 },   // SCALEOPP2
    {  43,     51,     55,     57 },   // SCALEZONE1_X
    {  11,     13,     14,     14 },   // SCALEZONE1_Y
    {  26,     17,     12,     10 },   // ZONE1OFFSET_X
    {   7,      4,      3,      3 }    // ZONE1OFFSET_Y
};

/* +1 to account for dummy nodes added because some trees are incomplete. */
const int ff_vc1_ac_sizes[AC_MODES] = {
    186, 169, 133, 149, 103 + 1, 103 + 1, 163, 175
};

const uint8_t ff_vc1_ac_coeff_lens[] = {
    /* High Motion Intra Tables (#177-179) - 186 entries */
     4,  7,  9, 11, 12, 14, 14, 14, 15, 15, 10,  8,  9,  9,  8,  9, 11, 13, 13,
    13, 13, 12, 12, 14, 14, 13, 12,  8,  5,  8,  8,  7,  9, 10, 13, 13, 12, 14,
    15, 15, 13, 13, 14, 14,  9,  9,  7, 10, 13, 13, 13, 13, 11, 10, 10, 13, 14,
    15, 15, 12, 12, 12, 10,  9,  7,  8,  8, 10, 11, 13, 14, 15, 15, 12, 11, 11,
    11, 13, 14, 14, 12,  8,  6, 11, 11, 12, 12, 11,  9,  8,  7,  6, 11, 14, 14,
    13, 12, 11, 13, 13, 13, 13,  9,  9, 12, 13, 13, 12, 12, 12, 12, 15, 15, 14,
    13, 13, 13,  9, 10, 12, 12, 11,  8,  2,  5,  7,  7, 12, 13, 14, 14, 11, 10,
    10, 10,  8,  8, 10, 11, 12, 14, 14, 14, 14,  9,  5,  5,  3,  6,  8,  8,  7,
     5,  4,  4,  7,  7,  6, 11, 11, 10, 11, 12, 12, 10, 10, 14, 14, 13, 13, 13,
    11,  9,  9, 11, 13, 13, 13, 14, 14, 11, 12, 13, 13,  8,  6,
    /* High Motion Inter Tables (#184-186) - 169 entries */
     3,  7,  9, 10, 10,  8, 10, 12, 13, 13, 13, 13, 12,  9,  8,  8, 11, 14, 14,
    13, 13, 15, 15, 14, 11, 12, 14, 14, 13,  9,  5,  4,  4,  6,  8,  8,  9,  9,
     8,  5,  3,  7, 10, 11, 12, 12, 10, 13, 14, 14, 12, 13, 15, 15, 14, 13, 13,
     8,  7, 10, 11, 11, 10, 10,  9, 10, 10,  5,  4,  8,  8,  8, 11, 11, 13, 15,
    15, 14, 12, 11, 10, 10,  6,  6,  6,  7,  7,  9,  9, 11, 13, 13, 14, 15, 15,
    14, 15, 15, 12, 12, 11,  9,  8,  9, 11, 12, 12, 11, 14, 14, 13, 12,  9, 10,
    10, 14, 14, 13, 14, 14, 13, 11, 12, 13, 14, 14, 11,  9,  7,  6,  5,  5,  4,
     6,  8, 10, 10, 11, 13, 13, 12, 11, 12, 12, 10, 10, 12, 12, 11, 11, 11,  9,
    10, 14, 15, 15, 14, 14, 13, 14, 14, 12, 13, 14, 14,  5,  5,  6,  6,
    /* Low Motion Intra Tables (#191-193) - 133 entries */
     7, 10, 10, 11, 11, 11, 13, 13, 13, 13,  8,  7,  8,  8,  8,  9, 11, 12, 13,
    13, 12, 13, 13, 13, 13, 12,  9,  9,  8,  7,  9, 11, 12, 12, 12, 12, 11,  8,
     4,  5,  7,  8,  9, 10, 10,  7,  8,  9, 13, 13, 12, 11, 12, 12, 11,  7, 11,
    11, 11, 12, 12, 10, 10,  8,  8,  9, 11, 11, 10,  7,  6, 10, 11, 11, 11, 12,
    12, 10,  9, 10, 12, 12, 11,  7,  2,  6,  7,  7,  5,  5,  8, 10, 10, 11, 11,
    12, 12, 12, 12,  8,  9, 11, 11, 11, 12, 12,  7,  8,  9, 10, 11, 12, 12,  4,
     5,  5,  3,  8,  8,  7,  6,  8, 10, 10,  9,  8,  9, 11, 12, 12, 10,  6,  4,
    /* Low Motion Inter Tables (#198-200) - 149 entries */
    10, 11, 11, 11, 12, 13, 14, 15, 15, 11, 11,  9,  9,  7,  8, 10, 12, 12, 13,
    14, 14, 12,  9, 12, 12, 11, 13, 13, 12, 12, 13, 13,  9, 10, 11, 12, 13, 14,
    14, 10, 10,  6,  7,  9, 10, 12, 13, 15, 15, 14, 11,  8,  6,  7, 10, 12, 12,
    12, 14, 14, 13,  9,  9, 10, 13, 14, 15, 15, 13, 14, 14, 11,  6,  6,  5,  7,
     7,  7,  7, 11, 11, 11, 11,  9,  9, 13, 13, 13, 14, 14, 11, 12, 12, 13, 13,
    12,  7,  6,  5,  4,  4,  5,  6,  9,  9,  8,  7,  5,  6,  9, 10, 10,  8,  8,
     8,  3,  5,  7,  9, 13, 14, 14, 12, 12, 13, 13, 10,  8, 13, 13, 12, 12, 12,
    11, 11, 13, 14, 14, 12, 13, 14, 14, 13, 13, 10,  8,  7,  4,  2,
    /* Mid Rate Intra Tables (#205-207) - 103 + 1 entries */
    -9, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11,
    11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12,  7, 10, 10,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
     9,  9,  9,  9,  9,  9,  9,  9,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
     8,  8,  7,  7,  7,  7,  7,  7,  7,  7,  6,  6,  6,  6,  6,  6,  6,  6,  6,
     6,  5,  5,  5,  4,  2,  3,  4,  4,
    /* Mid Rate Inter Tables (#212-214) - 103 + 1 entries */
    -9, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11,
    11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12,  7, 10, 10,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
     9,  9,  9,  9,  9,  9,  9,  9,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
     8,  8,  7,  7,  7,  7,  7,  7,  7,  7,  6,  6,  6,  6,  6,  6,  6,  6,  6,
     6,  5,  5,  5,  4,  2,  3,  4,  4,
    /* High Rate Intra Tables (#219-221) - 163 entries */
     2,  5,  8, 12, 12, 11, 13, 13, 12, 11, 10, 10,  7,  7,  9, 11, 11, 14, 14,
    13, 12, 11,  8,  4,  3,  5,  6,  9, 12, 12, 11, 10, 10, 11, 12, 12,  9,  7,
     4,  6,  6, 10, 10,  9,  8, 12, 12, 11, 11, 12, 12,  9,  9, 10, 10,  8,  8,
    11, 12, 12, 11, 11,  9,  8,  5,  6, 13, 13, 12, 11, 10,  9, 10, 10,  9,  7,
     9, 11, 12, 12, 10,  8,  9,  9,  8,  7,  7, 11, 13, 13, 15, 16, 16, 14, 13,
    10, 10, 11, 12, 13, 13,  8,  7, 11, 11, 10,  9,  8,  7,  4,  5,  6, 12, 12,
    12, 12, 10, 10, 10,  9, 11, 14, 14, 13, 12, 11, 11,  9,  9,  8,  7, 10, 12,
    12, 13, 15, 15, 14, 12,  9,  8,  7,  7, 11, 11, 10, 10, 13, 13, 12, 11,  9,
    11, 13, 13, 12, 10,  8,  9, 11, 11, 10,  6,
    /* High Rate Inter Tables (#226-228) - 175 entries */
     3,  5,  7, 10, 11, 12, 13, 13, 10, 11, 13, 13, 13, 13,  9,  9,  6,  6,  7,
    11, 11, 10,  9,  8,  5,  7,  7,  8,  8, 13, 13, 12, 11, 10, 12, 12, 11, 10,
     8,  6,  6,  5,  9, 10, 11, 14, 18, 21, 22, 22, 20, 19, 17, 16, 15, 13, 12,
    11, 11, 10, 10, 12, 13, 14, 18, 18, 17, 16, 15, 12, 12,  9,  9, 11, 12, 13,
    13, 10, 10, 11, 11,  7,  8,  8,  3,  2,  4,  7, 12, 13, 13, 11, 12, 14, 15,
    15, 13, 11, 11, 12, 12, 11, 12, 12,  9,  9,  6,  5,  7,  8, 13, 18, 20, 21,
    23, 23, 22, 19, 17, 16, 15, 14, 12, 11, 10,  9,  8,  8,  7,  8, 12, 14, 14,
    13, 11, 10,  9,  7,  9,  9, 12, 13, 14, 14, 11, 10,  9,  7,  5, 10, 10,  9,
    12, 13, 13, 12, 13, 13, 10, 10, 11, 13, 17, 18, 20, 20, 20, 20, 16, 15, 15,
    15, 12,  7,  6,
};

#define ESCAPE 0
const uint16_t ff_vc1_ac_coeff_syms[] = {
    /* High Motion Intra Tables (#177-179) - 186 entries */
    0x0201, 0x0601, 0x0404, 0x0604, 0x1201, 0x0806, 0x0323, 0x021A, 0x1802,
    0x0704, 0x0802, 0x0E00, 0x0501, 0x0A01, 0x0306, 0x020D, 0x0A02, 0x0608,
    0x031F, 0x0805, 0x1002, 0x0318, 0x0319, 0x0902, 0x2400, 0x2000, 0x0315,
    0x0307, 0x0202, 0x0801, 0x0209, 0x0402, 0x020C, 0x020E, 0x0507, 0x031C,
    0x1C00, 0x1402, 0x1C01, 0x0410, 0x0901, 0x060C, 0x060F, 0x0903, ESCAPE,
    0x1200, 0x0206, 0x0407, 0x0216, 0x0215, 0x1601, 0x050C, 0x0313, 0x0502,
    0x020F, 0x0E03, 0x1A01, 0x1603, 0x0411, 0x0212, 0x0316, 0x0213, 0x0503,
    0x0405, 0x0C00, 0x0308, 0x030D, 0x1600, 0x1A00, 0x040E, 0x021D, 0x021E,
    0x1E01, 0x0317, 0x040B, 0x040A, 0x0701, 0x0B00, 0x021B, 0x1602, 0x0702,
    0x0309, 0x0204, 0x0605, 0x0312, 0x0607, 0x0E02, 0x040C, 0x0603, 0x020A,
    0x0207, 0x0A00, 0x1001, 0x0A05, 0x0A04, 0x060B, 0x0505, 0x0210, 0x0219,
    0x0217, 0x031E, 0x0322, 0x030E, 0x030F, 0x031D, 0x0508, 0x060A, 0x1E00,
    0x0703, 0x031A, 0x1401, 0x0C04, 0x060E, 0x0807, 0x050A, 0x0218, 0x1202,
    0x0406, 0x040D, 0x0C03, 0x040F, 0x0314, 0x1000, 0x0200, 0x0401, 0x0304,
    0x0500, 0x0804, 0x050B, 0x0808, 0x021C, 0x0A03, 0x0310, 0x0803, 0x0E01,
    0x030A, 0x030B, 0x0409, 0x0900, 0x0506, 0x0325, 0x0B01, 0x0809, 0x0D00,
    0x1400, 0x0800, 0x0203, 0x0400, 0x0302, 0x0602, 0x020B, 0x0305, 0x0301,
    0x0600, 0x0300, 0x0403, 0x0208, 0x0205, 0x050D, 0x0606, 0x0408, 0x0211,
    0x0214, 0x031B, 0x1800, 0x0311, 0x1203, 0x1403, 0x1801, 0x1003, 0x0321,
    0x0504, 0x0700, 0x0C01, 0x0C02, 0x0509, 0x2200, 0x050F, 0x2600, 0x0324,
    0x060D, 0x0609, 0x0320, 0x050E, 0x030C, 0x0303,
    /* High Motion Inter Tables (#184-186) - 169 entries */
    0x0200, 0x0308, 0x020A, 0x0603, 0x020B, 0x0207, 0x0310, 0x0316, 0x0C02,
    0x0901, 0x031F, 0x0804, 0x0507, 0x0602, 0x0403, 0x030B, 0x0318, 0x0608,
    0x0C03, 0x0607, 0x2000, 0x0E03, 0x0B01, 0x050B, 0x0409, 0x040A, 0x031D,
    0x0E02, 0x040B, 0x0404, 0x0202, 0x0400, 0x0201, 0x0800, 0x0501, 0x030C,
    0x1000, 0x0502, 0x030D, 0x0600, 0x0300, 0x0402, 0x020C, 0x0604, 0x0702,
    0x1C00, 0x1400, 0x0D00, 0x2600, 0x1401, 0x0211, 0x0805, 0x1300, 0x2C00,
    0x031E, 0x050A, 0x2200, 0x0209, 0x0601, 0x0313, 0x1800, 0x0214, 0x0312,
    0x0504, 0x0405, 0x0315, 0x0406, 0x0303, 0x0301, 0x030E, 0x0208, 0x0E00,
    0x0323, 0x0319, 0x050E, 0x0903, 0x021A, 0x0705, 0x0508, 0x0803, 0x0900,
    0x0802, 0x0204, 0x0500, 0x0306, 0x0206, 0x030A, 0x0314, ESCAPE, 0x0605,
    0x1201, 0x0322, 0x0902, 0x0706, 0x060B, 0x2800, 0x2E00, 0x0410, 0x1E00,
    0x031C, 0x020F, 0x0503, 0x0801, 0x1200, 0x0506, 0x0509, 0x0215, 0x0408,
    0x0216, 0x1100, 0x0704, 0x0703, 0x030F, 0x1600, 0x0407, 0x0807, 0x0A04,
    0x0609, 0x0321, 0x1601, 0x0320, 0x1A00, 0x1001, 0x2400, 0x0217, 0x0218,
    0x0A02, 0x0A01, 0x0C00, 0x0307, 0x0203, 0x0304, 0x0302, 0x0205, 0x0700,
    0x0505, 0x0C01, 0x031B, 0x040D, 0x050C, 0x050D, 0x0E01, 0x031A, 0x0A03,
    0x0701, 0x020D, 0x0213, 0x0212, 0x0B00, 0x0317, 0x0210, 0x0311, 0x020E,
    0x2A00, 0x1002, 0x040F, 0x0A05, 0x040E, 0x0F00, 0x060A, 0x0219, 0x0606,
    0x040C, 0x0324, 0x0806, 0x0305, 0x0401, 0x0309, 0x0A00,
    /* Low Motion Intra Tables (#191-193) - 133 entries */
    0x0303, 0x0407, 0x0802, 0x0605, 0x0310, 0x040B, 0x0316, 0x2000, 0x031A,
    0x1002, 0x0306, 0x0601, 0x0307, 0x0209, 0x0E00, 0x0A01, 0x0701, 0x1E00,
    0x0901, 0x0806, 0x0804, 0x0214, 0x1601, 0x050C, 0x0A04, 0x060A, 0x020D,
    0x020C, 0x0801, 0x0206, 0x0405, 0x0313, 0x1201, 0x0314, 0x0C03, 0x0317,
    0x1A00, 0x0309, 0x0201, 0x0202, 0x0402, 0x0308, 0x1200, 0x0503, 0x0409,
    ESCAPE, 0x030D, 0x0603, 0x0E03, 0x0508, 0x0315, 0x040C, 0x040F, 0x0212,
    0x0210, 0x0C00, 0x0606, 0x1001, 0x050D, 0x0319, 0x060B, 0x020F, 0x020E,
    0x030A, 0x020A, 0x030F, 0x0A02, 0x0A03, 0x0408, 0x0207, 0x0204, 0x1600,
    0x0C02, 0x0312, 0x060D, 0x0E02, 0x0506, 0x0803, 0x0406, 0x040D, 0x1401,
    0x0318, 0x1C00, 0x0304, 0x0200, 0x0A00, 0x0500, 0x0305, 0x0401, 0x0301,
    0x030B, 0x0502, 0x040A, 0x0609, 0x0211, 0x0703, 0x040E, 0x0507, 0x050B,
    0x1000, 0x030E, 0x0505, 0x0900, 0x0504, 0x0805, 0x0608, 0x0403, 0x0602,
    0x1400, 0x0E01, 0x0213, 0x0509, 0x050A, 0x0300, 0x0800, 0x0203, 0x0400,
    0x020B, 0x0404, 0x0208, 0x0302, 0x0501, 0x0604, 0x1800, 0x0700, 0x030C,
    0x0C01, 0x0607, 0x0702, 0x060C, 0x0311, 0x0205, 0x0600,
    /* Low Motion Inter Tables (#198-200) - 149 entries */
    0x0319, 0x0210, 0x0602, 0x0406, 0x0504, 0x0900, 0x050A, 0x050F, 0x0901,
    0x0407, 0x0801, 0x020B, 0x020E, 0x030A, 0x0209, 0x0315, 0x031F, 0x0603,
    0x0218, 0x050E, 0x050D, 0x0212, 0x0314, 0x040C, 0x0213, 0x0214, 0x0507,
    0x0509, 0x1000, 0x0215, 0x0C01, 0x032A, ESCAPE, 0x0502, 0x021B, 0x021A,
    0x040D, 0x0328, 0x0803, 0x0404, 0x0405, 0x0203, 0x0206, 0x020D, 0x0318,
    0x031E, 0x0508, 0x0703, 0x060A, 0x1800, 0x0503, 0x030F, 0x0306, 0x030C,
    0x031A, 0x040A, 0x021C, 0x0A01, 0x0609, 0x0325, 0x040E, 0x0312, 0x031B,
    0x031C, 0x0606, 0x0A02, 0x1C00, 0x1201, 0x0322, 0x1001, 0x0B00, 0x0E00,
    0x0307, 0x0309, 0x0303, 0x030B, 0x030E, 0x030D, 0x0600, 0x0409, 0x0323,
    0x0408, 0x0211, 0x0403, 0x0601, 0x1400, 0x0216, 0x0217, 0x021D, 0x060C,
    0x040B, 0x0506, 0x0219, 0x0329, 0x0607, 0x0802, 0x0207, 0x0308, 0x0202,
    0x0302, 0x0301, 0x0305, 0x0204, 0x0313, 0x020F, 0x0402, 0x0401, 0x0304,
    0x0205, 0x0501, 0x0C00, 0x0317, 0x020A, 0x0310, 0x0800, 0x0200, 0x0400,
    0x0500, 0x0A00, 0x0702, 0x0608, 0x0326, 0x0701, 0x1200, 0x0321, 0x0324,
    0x0700, 0x0311, 0x040F, 0x060B, 0x0320, 0x050C, 0x0605, 0x0505, 0x031D,
    0x0E01, 0x1A00, 0x0804, 0x0604, 0x032B, 0x0327, 0x0805, 0x050B, 0x1600,
    0x0316, 0x020C, 0x0208, 0x0201, 0x0300,
    /* Mid Rate Intra Tables (#205-207) - 103 + 1 entries */
         0, 0x0F00, 0x0D00, 0x2C00, 0x2A00, 0x0502, 0x0701, 0x0B00, 0x020D,
    0x0605, 0x0408, 0x0604, 0x0803, 0x0802, 0x0E01, 0x2800, 0x2600, 0x2E00,
    0x3000, 0x1001, 0x0409, 0x0503, 0x0504, 0x030F, 0x0310, 0x3200, 0x3400,
    0x3600, 0x1201, 0x0606, 0x1401, 0x0A02, 0x0607, 0x020E, 0x1100, 0x0505,
    0x0506, 0x0311, 0x0312, 0x0313, 0x0314, ESCAPE, 0x2400, 0x2200, 0x030E,
    0x030D, 0x030C, 0x030B, 0x030A, 0x0501, 0x0900, 0x020C, 0x020B, 0x0407,
    0x0406, 0x0405, 0x0603, 0x0602, 0x0C01, 0x0A01, 0x2000, 0x0404, 0x1E00,
    0x1C00, 0x1A00, 0x0308, 0x0307, 0x0306, 0x0700, 0x020A, 0x0209, 0x0208,
    0x0309, 0x0403, 0x0801, 0x1800, 0x1600, 0x1400, 0x0304, 0x0303, 0x0206,
    0x0305, 0x0207, 0x0402, 0x0601, 0x1200, 0x0500, 0x0205, 0x0302, 0x0301,
    0x0204, 0x0203, 0x1000, 0x0E00, 0x0401, 0x0C00, 0x0202, 0x0A00, 0x0800,
    0x0300, 0x0200, 0x0400, 0x0201, 0x0600,
    /* Mid Rate Inter Tables (#212-214) - 103 + 1 entries */
         0, 0x0501, 0x0700, 0x1600, 0x1400, 0x031C, 0x031B, 0x031A, 0x0319,
    0x0409, 0x0408, 0x0407, 0x0406, 0x0405, 0x0603, 0x0602, 0x0801, 0x1800,
    0x0A01, 0x0217, 0x0218, 0x031D, 0x031E, 0x031F, 0x0320, 0x0C01, 0x0802,
    0x0604, 0x0605, 0x0606, 0x040A, 0x0219, 0x021A, 0x0321, 0x0322, 0x0323,
    0x0324, 0x0325, 0x0326, 0x0327, 0x0328, ESCAPE, 0x1200, 0x1000, 0x0318,
    0x0317, 0x0316, 0x0315, 0x0314, 0x0313, 0x0312, 0x0311, 0x0500, 0x0216,
    0x0215, 0x0214, 0x0213, 0x0212, 0x0211, 0x0210, 0x020F, 0x0404, 0x0403,
    0x0E00, 0x0C00, 0x0310, 0x030F, 0x030E, 0x030D, 0x030C, 0x030B, 0x030A,
    0x0309, 0x020E, 0x020D, 0x0402, 0x0601, 0x0A00, 0x0308, 0x0307, 0x0306,
    0x0305, 0x020C, 0x020B, 0x020A, 0x0800, 0x0304, 0x0303, 0x0302, 0x0301,
    0x0209, 0x0208, 0x0207, 0x0206, 0x0401, 0x0600, 0x0205, 0x0204, 0x0203,
    0x0300, 0x0200, 0x0201, 0x0202, 0x0400,
    /* High Rate Intra Tables (#219-221) - 163 entries */
    0x0200, 0x0202, 0x0207, 0x6A00, 0x1402, 0x0502, 0x040B, 0x0507, 0x6C00,
    0x030E, 0x4600, 0x1801, 0x0205, 0x1E00, 0x1201, 0x5800, 0x1E01, 0x0509,
    0x0702, 0x0506, 0x0504, 0x0804, 0x2800, 0x0800, 0x0400, 0x0401, 0x1400,
    0x0405, 0x040A, 0x0A04, 0x0408, 0x4400, 0x030B, 0x5600, 0x6800, 0x2601,
    0x3400, 0x1C00, 0x0201, 0x0203, 0x0601, 0x4200, 0x0803, 0x0603, 0x2600,
    0x2401, 0x0607, 0x5400, 0x0700, 0x0805, 0x030F, 0x0500, 0x0306, 0x1601,
    0x0604, 0x0302, 0x0C01, 0x1C01, 0x0503, 0x6600, 0x5200, 0x0A03, 0x3200,
    0x0602, 0x0C00, 0x1200, 0x0A05, 0x0C04, 0x1202, 0x020C, 0x4000, 0x1001,
    0x020A, 0x0309, 0x0802, 0x1A00, 0x0208, 0x5000, 0x6400, 0x6200, 0x3E00,
    0x2400, 0x0305, 0x3000, 0x0403, 0x0801, 0x0402, 0x0E02, 0x0505, 0x0608,
    0x040D, 0x050E, 0x050C, 0x040C, 0x0701, 0x0406, 0x030A, 0x0605, 0x6000,
    0x0806, 0x0900, 0x0206, 0x0204, 0x030D, 0x4E00, 0x3C00, 0x2E00, 0x2200,
    0x1800, 0x0600, 0x0A00, 0x1000, 0x2201, 0x0409, 0x020D, 0x0C03, 0x1401,
    0x0308, 0x0A02, 0x0404, 0x4C00, 0x0508, 0x050A, 0x7000, 0x5E00, 0x1A01,
    0x4A00, 0x0304, 0x2C00, 0x2000, 0x0300, 0x3A00, 0x5C00, 0x2001, 0x2801,
    0x050D, 0x050B, 0x0609, 0x5A00, 0x0E01, 0x0A01, ESCAPE, 0x1600, 0x4800,
    0x020B, 0x3800, 0x0209, 0x0310, 0x6E00, 0x1002, 0x030C, 0x2A00, 0x0407,
    0x020E, 0x0E03, 0x0606, 0x0307, 0x0301, 0x0303, 0x0501, 0x0C02, 0x3600,
    0x0E00,
    /* High Rate Inter Tables (#226-228) - 175 entries */
    0x0400, 0x0800, 0x0306, 0x0313, 0x1201, 0x3400, 0x0900, 0x0505, 0x020F,
    0x2800, 0x1002, 0x4000, 0x1A01, 0x0218, 0x020D, 0x1800, 0x0302, 0x0206,
    0x0E00, 0x0212, 0x0318, 0x0312, 0x0500, 0x0309, 0x0204, 0x0208, 0x0305,
    0x0602, 0x020A, 0x3E00, 0x031E, 0x0216, 0x0317, 0x0501, 0x3200, 0x031C,
    0x0502, 0x0311, 0x1200, 0x0A00, 0x0301, 0x0401, 0x030E, 0x1E00, 0x2600,
    0x0806, 0x0412, 0x0516, 0x051B, 0x051A, 0x0515, 0x0513, 0x050E, 0x050C,
    0x0509, 0x3C00, 0x0409, 0x0211, 0x0803, 0x0406, 0x0E01, 0x0605, 0x040A,
    0x040C, 0x050F, 0x0413, 0x0410, 0x050A, 0x0609, 0x3000, 0x1401, 0x030D,
    0x1600, 0x0604, 0x031B, 0x0701, 0x0217, 0x1C00, 0x0310, 0x0316, 0x0210,
    0x0304, 0x0308, 0x1000, 0x0201, 0x0200, 0x0202, 0x0207, 0x2E00, 0x031D,
    0x3A00, 0x2400, 0x0503, 0x0A04, 0x040D, 0x050B, 0x1801, 0x0407, 0x1001,
    0x0215, 0x0319, 0x0315, 0x2C00, 0x031A, 0x030C, 0x020C, 0x0205, 0x0203,
    0x0C00, 0x0403, 0x3800, 0x0411, 0x0514, 0x0517, 0x051C, 0x0518, 0x0519,
    0x0414, 0x040F, 0x040E, 0x0508, 0x0805, 0x0C02, 0x2200, 0x020E, 0x030B,
    0x0307, 0x0801, ESCAPE, 0x0209, 0x0700, 0x0506, 0x0607, 0x0606, 0x0314,
    0x0603, 0x1400, 0x0303, 0x0404, 0x0A01, 0x0214, 0x3600, 0x040B, 0x0C03,
    0x0A02, 0x1A00, 0x030A, 0x0601, 0x0600, 0x0802, 0x0405, 0x020B, 0x0408,
    0x0504, 0x0A03, 0x2A00, 0x0E02, 0x0804, 0x0C01, 0x030F, 0x2000, 0x1601,
    0x050D, 0x0510, 0x0415, 0x0511, 0x0416, 0x0512, 0x0703, 0x0702, 0x0608,
    0x0507, 0x0213, 0x0402, 0x0300,
};
#undef ESCAPE
