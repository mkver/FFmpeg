/*
 * MJPEG encoder
 * Copyright (c) 2016 William Ma, Ted Ying, Jerry Jiang
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

#define __STDC_WANT_IEC_60559_BFP_EXT__
#include <limits.h>
#ifndef UINT_WIDTH
#define UINT_WIDTH (CHAR_BIT * sizeof(unsigned))
#endif

#include <string.h>
#include <stdint.h>
#include "libavutil/avassert.h"
#include "libavutil/intmath.h"
#include "libavutil/qsort.h"
#include "mjpegenc_huffman.h"

/**
 * Used to assign a occurrence count or "probability" to an input value
 */
typedef struct PTable {
    int value;  ///< input value
    int prob;   ///< number of occurences of this value in input
} PTable;

/**
 * Used to store intermediate lists in the package merge algorithm
 */
typedef struct PackageMergerList {
    int nitems;             ///< number of items in the list and probability      ex. 4
    int item_idx[515];      ///< index range for each item in items                   0, 2, 5, 9, 13
    int probability[514];   ///< probability of each item                             3, 8, 18, 46
    int items[257 * 16];    ///< chain of all individual values that make up items    A, B, A, B, C, A, B, C, D, C, D, D, E
} PackageMergerList;

/**
 * Comparison function for two PTables by prob
 *
 * @param a First PTable to compare
 * @param b Second PTable to compare
 * @return < 0 for less than, 0 for equals, > 0 for greater than
 */
static int compare_by_prob(const void *a, const void *b)
{
    PTable a_val = *(PTable *) a;
    PTable b_val = *(PTable *) b;
    return a_val.prob - b_val.prob;
}

/**
 * Computes the length of the Huffman encoding for each distinct input value.
 * Uses package merge algorithm as follows:
 * 1. start with an empty list, lets call it list(0), set i = 0
 * 2. add 1 entry to list(i) for each symbol we have and give each a score equal to the probability of the respective symbol
 * 3. merge the 2 symbols of least score and put them in list(i+1), and remove them from list(i). The new score will be the sum of the 2 scores
 * 4. if there is more than 1 symbol left in the current list(i), then goto 3
 * 5. i++
 * 6. if i < 16 goto 2
 * 7. select the n-1 elements in the last list with the lowest score (n = the number of symbols)
 * 8. the length of the huffman code for symbol s will be equal to the number of times the symbol occurs in the select elements
 * Go to guru.multimedia.cx/small-tasks-for-ffmpeg/ for more details
 *
 * All probabilities should be nonnegative integers.
 *
 * @param prob_table[in,out] array of a PTable for each distinct input value,
 *                           will be sorted according to ascending probability
 * @param counts[out]        the number of values of a given length
 * @param size               number of elements of the prob_table array
 * @param max_length         max length of a code
 */
static void mjpegenc_huffman_compute_bits(PTable *prob_table,
                                          uint8_t counts[/* max_length + 1 */],
                                          int size, int max_length)
{
    PackageMergerList list_a, list_b, *to = &list_a, *from = &list_b, *temp;

    int times, i, j, k;

    int nbits[257] = {0};

    int min;

    av_assert0(max_length > 0);

    to->nitems = 0;
    from->nitems = 0;
    to->item_idx[0] = 0;
    from->item_idx[0] = 0;
    AV_QSORT(prob_table, size, PTable, compare_by_prob);

    for (times = 0; times <= max_length; times++) {
        to->nitems = 0;
        to->item_idx[0] = 0;

        j = 0;
        k = 0;

        if (times < max_length) {
            i = 0;
        }
        while (i < size || j + 1 < from->nitems) {
            to->nitems++;
            to->item_idx[to->nitems] = to->item_idx[to->nitems - 1];
            if (i < size &&
                (j + 1 >= from->nitems ||
                 prob_table[i].prob <
                     from->probability[j] + from->probability[j + 1])) {
                to->items[to->item_idx[to->nitems]++] = prob_table[i].value;
                to->probability[to->nitems - 1] = prob_table[i].prob;
                i++;
            } else {
                for (k = from->item_idx[j]; k < from->item_idx[j + 2]; k++) {
                    to->items[to->item_idx[to->nitems]++] = from->items[k];
                }
                to->probability[to->nitems - 1] =
                    from->probability[j] + from->probability[j + 1];
                j += 2;
            }
        }
        temp = to;
        to = from;
        from = temp;
    }

    min = (size - 1 < from->nitems) ? size - 1 : from->nitems;
    for (i = 0; i < from->item_idx[min]; i++) {
        nbits[from->items[i]]++;
    }
    // we don't want to return the 256 bit count (it was just in here to prevent
    // all 1s encoding)
    memset(counts, 0, sizeof(counts[0]) * (max_length + 1));
    for (int i = 0; i < 256; ++i)
        counts[nbits[i]]++;
}

void ff_mjpeg_encode_huffman_init(MJpegEncHuffmanContext *s)
{
    memset(s->val_count, 0, sizeof(s->val_count));
}

void ff_mjpeg_encode_huffman_close(const MJpegEncHuffmanContext *s, uint8_t bits[17],
                                   uint8_t val[], int max_nval,
                                   uint8_t huff_len[], uint16_t huff_code[])
{
    PTable val_counts[257];

    av_assert1(max_nval <= FF_ARRAY_ELEMS(val_counts) - 1);

    int nval = 0;
    for (int i = 0; i < 256; i++) {
        if (s->val_count[i]) {
            val_counts[nval].value = i;
            val_counts[nval].prob  = s->val_count[i];
            nval++;
            av_assert2(nval <= max_nval);
        }
    }
    val_counts[nval].value = 256;
    val_counts[nval].prob  = 0;

    mjpegenc_huffman_compute_bits(val_counts, bits, nval + 1, 16);

    // val_counts[0] is the fake element we added earlier.
    av_assert1(val_counts[0].prob == 0 && val_counts[0].value == 256);
    // The following loop puts the values with higher occurence first,
    // ensuring that they get the shorter codes.
    unsigned code = 0;
    for (int len = 1, i = 0; len <= 16; ++len) {
        // Values with the same length are ordered so that codes which are
        // likely to lead to 0xFF escape values (i.e. codes with initial
        // and trailing 1 bits) occur with lower frequency.
        uint8_t penalty[255];
        uint8_t penalty_counts[16] = { 0 };
        unsigned offsets[16];

        for (int j = 0; j < bits[len]; ++j) {
            // Negate j + code to be able to use ff_ctz()/ff_clz().
            unsigned tmp = ~(j + code);

            // Due to the fake element j + code is not the length len 1...1 code.
            av_assert2(tmp & ((1 << len) - 1));

            penalty[j] = ff_ctz(tmp) + ff_clz(tmp << (UINT_WIDTH - len));

            // tmp has a 1 bit among its lowest len bits. This also ensures
            // that no zero bit is counted twice.
            av_assert2(penalty[j] < len);
            penalty_counts[penalty[j]]++;
        }

        offsets[0] = 0;
        for (size_t j = 1; j < FF_ARRAY_ELEMS(offsets); ++j)
            offsets[j] = penalty_counts[j - 1] + offsets[j - 1];

        for (int j = 0; j < bits[len]; ++j) {
            unsigned idx = offsets[penalty[j]]++;
            unsigned sym = val[i + j] = val_counts[nval - (i + idx)].value;
            huff_len[sym]  = len;
            huff_code[sym] = code++;
        }

        i += bits[len];
        code <<= 1;
    }
}
