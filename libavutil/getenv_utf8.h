/*
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

#ifndef AVUTIL_GETENV_UTF8_H
#define AVUTIL_GETENV_UTF8_H

#include <stdlib.h>

#include "config.h"
#include "mem.h"

#if HAVE_GETENV

#ifdef _WIN32

#include "libavutil/wchar_filename.h"

static inline char *getenv_utf8(const char *varname)
{
    wchar_t *varname_w, *var_w;
    char *var;

    if (utf8towchar(varname, &varname_w))
        return NULL;
    if (!varname_w)
        return NULL;

    var_w = _wgetenv(varname_w);
    av_free(varname_w);

    if (!var_w)
        return NULL;
    if (wchartoutf8(var_w, &var))
        return NULL;

    return var;

    // No CP_ACP fallback compared to other *_utf8() functions:
    // non UTF-8 strings must not be returned.
}

static inline void freeenv_utf8(char *var)
{
    av_free(var);
}

static inline char *getenv_make_writable(char *var)
{
    return var;
}

#else

static inline char *getenv_utf8(const char *varname)
{
    return getenv(varname);
}

static inline void freeenv_utf8(char *var)
{
}

static inline char *getenv_make_writable(const char *var)
{
    return av_strdup(var);
}

#endif // _WIN32

#else

#define getenv_utf8(x) NULL

#define freeenv_utf8(x) ((void) 0)

#define getenv_make_writable(x) NULL

#endif // HAVE_GETENV

#endif // AVUTIL_GETENV_UTF8_H
