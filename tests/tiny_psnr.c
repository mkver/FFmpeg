/*
 * Copyright (c) 2003 Michael Niedermayer <michaelni@gmx.at>
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <math.h>
#include <float.h>
#include <limits.h>

#include "libavutil/intfloat.h"
#include "libavutil/intreadwrite.h"

enum SampleType {
    U8,
    S16LE,
    S24LE,
    S32LE,
    F32LE,
    F64LE,
};

#define FFMIN(a, b) ((a) > (b) ? (b) : (a))
#define F 100
// Needs to be divisible by all supported sizes
#define SIZE (3 * 1024)

uint64_t exp16_table[21] = {
           65537,
           65538,
           65540,
           65544,
           65552,
           65568,
           65600,
           65664,
           65793,
           66050,
           66568,
           67616,
           69763,
           74262,
           84150,
          108051,
          178145,
          484249,
         3578144,
       195360063,
    582360139072LL,
};

#if 0
// 16.16 fixpoint exp()
static unsigned int exp16(unsigned int a){
    int i;
    int out= 1<<16;

    for(i=19;i>=0;i--){
        if(a&(1<<i))
            out= (out*exp16_table[i] + (1<<15))>>16;
    }

    return out;
}
#endif

// 16.16 fixpoint log()
static int64_t log16(uint64_t a)
{
    int i;
    int out = 0;

    if (a < 1 << 16)
        return -log16((1LL << 32) / a);
    a <<= 16;

    for (i = 20; i >= 0; i--) {
        int64_t b = exp16_table[i];
        if (a < (b << 16))
            continue;
        out |= 1 << i;
        a    = ((a / b) << 16) + (((a % b) << 16) + b / 2) / b;
    }
    return out;
}

static uint64_t int_sqrt(uint64_t a)
{
    uint64_t ret    = 0;
    uint64_t ret_sq = 0;
    int s;

    for (s = 31; s >= 0; s--) {
        uint64_t b = ret_sq + (1ULL << (s * 2)) + (ret << s) * 2;
        if (b <= a) {
            ret_sq = b;
            ret   += 1ULL << s;
        }
    }
    return ret;
}

static uint8_t get_u8(const uint8_t *p)
{
    return *p;
}

static int16_t get_s16l(uint8_t *p)
{
    union {
        uint16_t u;
        int16_t  s;
    } v;
    v.u = p[0] | p[1] << 8;
    return v.s;
}

static int32_t get_s24l(const uint8_t *p)
{
    union {
        uint32_t u;
        int32_t  s;
    } v = { .u = AV_RL24(p) };
    v.u <<= 8;
    return v.s >> 8;
}

static int32_t get_s32l(const uint8_t *p)
{
    return AV_RL32(p);
}

static float get_f32l(uint8_t *p)
{
    union av_intfloat32 v;
    v.i = p[0] | p[1] << 8 | p[2] << 16 | p[3] << 24;
    return v.f;
}

static double get_f64l(uint8_t *p)
{
    return av_int2double(AV_RL64(p));
}

static int print_int(uint64_t maxdist, uint64_t sse, uint64_t nb_elems,
                     uint64_t size0, uint64_t size1, unsigned elem_len)
{
    const int64_t max = (1LL << (8 * elem_len)) - 1;
    uint64_t dev = int_sqrt(((sse / nb_elems) * F * F) + (((sse % nb_elems) * F * F) + nb_elems / 2) / nb_elems);
    uint64_t psnr;

    if (sse)
        psnr = ((2 * log16(max << 16) + log16(nb_elems) - log16(sse)) *
                284619LL * F + (1LL << 31)) / (1LL << 32);
    else
        psnr = 1000 * F - 1; // floating point free infinity :)

    printf("stddev:%5d.%02d PSNR:%3d.%02d MAXDIFF:%5"PRIu64" bytes:%9"PRIu64"/%9"PRIu64"\n",
            (int)(dev / F), (int)(dev % F),
            (int)(psnr / F), (int)(psnr % F),
            maxdist, size0, size1);
    return psnr;
}

static int print_float(double maxdist_d, double sse, uint64_t nb_elems,
                       uint64_t size0, uint64_t size1, unsigned elem_len)
{
    char psnr_str[64];
    double psnr = INT_MAX;
    double dev = sqrt(sse / nb_elems);
    uint64_t scale = (elem_len == 4) ? (1ULL << 24) : (1ULL << 32);
    uint64_t maxdist = maxdist_d * scale;

    if (sse) {
        psnr = 2 * log(DBL_MAX) - log(nb_elems / sse);
        snprintf(psnr_str, sizeof(psnr_str), "%5.02f", psnr);
    } else
        snprintf(psnr_str, sizeof(psnr_str), "inf");

    printf("stddev:%10.2f PSNR:%s MAXDIFF:%10"PRIu64" bytes:%9"PRIu64"/%9"PRIu64"\n",
            dev * scale, psnr_str, maxdist, size0, size1);
    return psnr;
}

#define PSNR_FUNC(suffix, elem_len, BASE_TYPE, SQUARE_TYPE, SSE_TYPE, abs_func, print_func) \
static int run_psnr_ ## suffix(FILE *const f[2])                             \
{                                                                            \
    uint8_t buf[2][SIZE];                                                    \
    uint64_t size0 = 0, size1 = 0;                                           \
    BASE_TYPE maxdist = 0;                                                   \
    SSE_TYPE sse = 0;                                                        \
    uint64_t nb_elems;                                                       \
                                                                             \
    while (1) {                                                              \
        int s0 = fread(buf[0], 1, SIZE, f[0]);                               \
        int s1 = fread(buf[1], 1, SIZE, f[1]);                               \
                                                                             \
        for (int j = 0; j < FFMIN(s0, s1); j += elem_len) {                  \
            BASE_TYPE a = get_ ## suffix(buf[0] + j);                        \
            BASE_TYPE b = get_ ## suffix(buf[1] + j);                        \
            BASE_TYPE dist = a - b;                                          \
            dist = abs_func(dist);                                           \
            sse += (SQUARE_TYPE)dist * dist;                                 \
            if (dist > maxdist)                                              \
                maxdist = dist;                                              \
        }                                                                    \
        size0 += s0;                                                         \
        size1 += s1;                                                         \
        if (s0 + s1 <= 0)                                                    \
            break;                                                           \
    }                                                                        \
                                                                             \
    nb_elems = FFMIN(size0, size1) / elem_len;                               \
    if (!nb_elems)                                                           \
        nb_elems = 1;                                                        \
    return print_func(maxdist, sse, nb_elems, size0, size1, elem_len);       \
}

PSNR_FUNC(u8, 1, int, int, uint64_t, abs, print_int)
PSNR_FUNC(s16l, 2, int, unsigned, uint64_t, abs, print_int)
PSNR_FUNC(s24l, 3, int, int64_t, uint64_t, abs, print_int)
PSNR_FUNC(s32l, 4, int64_t, int64_t, uint64_t, llabs, print_int)
PSNR_FUNC(f32l, 4, double, double, double, fabs, print_float)
PSNR_FUNC(f64l, 8, double, double, double, fabs, print_float)

static int run_psnr(FILE *f[2], enum SampleType type, int shift, int skip_bytes)
{
    uint64_t i;
    int noseek;

    noseek = fseek(f[0], 0, SEEK_SET) ||
             fseek(f[1], 0, SEEK_SET);

    if (!noseek) {
        for (i = 0; i < 2; i++) {
            uint8_t p[12];
            if (fread(p, 1, 12, f[i]) != 12)
                return -1;
            if (!memcmp(p, "RIFF", 4) &&
                !memcmp(p + 8, "WAVE", 4)) {
                if (fread(p, 1, 8, f[i]) != 8)
                    return -1;
                while (memcmp(p, "data", 4)) {
                    int s = p[4] | p[5] << 8 | p[6] << 16 | p[7] << 24;
                    fseek(f[i], s, SEEK_CUR);
                    if (fread(p, 1, 8, f[i]) != 8)
                        return -1;
                }
            } else {
                fseek(f[i], -12, SEEK_CUR);
            }
        }

        fseek(f[shift < 0], abs(shift), SEEK_CUR);

        fseek(f[0], skip_bytes, SEEK_CUR);
        fseek(f[1], skip_bytes, SEEK_CUR);
    }

    switch (type) {
    case    U8: return run_psnr_u8  (f);
    case S16LE: return run_psnr_s16l(f);
    case S24LE: return run_psnr_s24l(f);
    case S32LE: return run_psnr_s32l(f);
    case F32LE: return run_psnr_f32l(f);
    case F64LE: return run_psnr_f64l(f);
    }
    return -1;
}

int main(int argc, char *argv[])
{
    FILE *f[2];
    enum SampleType type = U8;
    int shift_first= argc < 5 ? 0 : atoi(argv[4]);
    int skip_bytes = argc < 6 ? 0 : atoi(argv[5]);
    int shift_last = shift_first + (argc < 7 ? 0 : atoi(argv[6]));
    int shift;
    int max_psnr   = -1;
    int max_psnr_shift = 0;

    if (shift_last > shift_first)
        shift_first -= shift_last - shift_first;

    if (argc > 3) {
        if (!strcmp(argv[3], "u8")) {
            type = U8;
        } else if (!strcmp(argv[3], "s16")) {
            type = S16LE;
        } else if (!strcmp(argv[3], "s24")) {
            type = S24LE;
        } else if (!strcmp(argv[3], "s32")) {
            type = S32LE;
        } else if (!strcmp(argv[3], "f32")) {
            type = F32LE;
        } else if (!strcmp(argv[3], "f64")) {
            type = F64LE;
        } else {
            char *end;
            long len = strtol(argv[3], &end, 0);
            if (*end || len < 1 || len > 4) {
                fprintf(stderr, "Unsupported sample format: %s\nSupported: u8, s16, s24, f32, f64\n", argv[3]);
                return 1;
            }
            switch (len) {
            case 1: type = U8; break;
            case 2: type = S16LE; break;
            case 3: type = S24LE; break;
            case 4: type = S32LE; break;
            }
        }
    }

    if (argc < 3) {
        printf("tiny_psnr <file1> <file2> [<elem size>|u8|s16|f32|f64 [<shift> [<skip bytes> [<shift search range>]]]]\n");
        printf("WAV headers are skipped automatically.\n");
        return 1;
    }

    f[0] = fopen(argv[1], "rb");
    f[1] = fopen(argv[2], "rb");
    if (!f[0] || !f[1]) {
        fprintf(stderr, "Could not open input files.\n");
        return 1;
    }

    for (shift = shift_first; shift <= shift_last; shift++) {
        int psnr = run_psnr(f, type, shift, skip_bytes);
        if (psnr > max_psnr || (shift < 0 && psnr == max_psnr)) {
            max_psnr = psnr;
            max_psnr_shift = shift;
        }
    }
    if (max_psnr < 0)
        return 2;

    if (shift_last > shift_first)
        printf("Best PSNR is %3d.%02d for shift %i\n", (int)(max_psnr / F), (int)(max_psnr % F), max_psnr_shift);
    return 0;
}
