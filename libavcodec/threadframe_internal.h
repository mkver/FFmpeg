/*
 * Copyright (c) 2022 Andreas Rheinhardt <andreas.rheinhardt@outlook.com>
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

#ifndef AVCODEC_THREADFRAME_INTERNAL_H
#define AVCODEC_THREADFRAME_INTERNAL_H

#include <stdatomic.h>

#include "libavutil/frame.h"

typedef struct ProgressInternal {
    atomic_int progress[2];
    void *owner[2];
    AVFrame *f;
} ProgressInternal;

int ff_progress_frame_pool_init_cb(void *opaque, void *data);

#endif /* AVCODEC_THREADFRAME_INTERNAL_H */
