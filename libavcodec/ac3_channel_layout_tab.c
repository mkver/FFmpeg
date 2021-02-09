/*
 * AC-3 channel layout table
 * copyright (c) 2001 Fabrice Bellard
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

#include "ac3_channel_layout_tab.h"

#include "version.h"
#if LIBAVCODEC_VERSION_MAJOR < 59
#undef AVCODEC_AC3_CHANNEL_LAYOUT_TAB_H
#define ff_ac3_channel_layout_tab avpriv_ac3_channel_layout_tab
#include "ac3_channel_layout_tab.h"
#endif
