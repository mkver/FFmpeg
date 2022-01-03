/*
 * AVPacket List API function implementations
 * Copyright (c) 2000, 2001, 2002 Fabrice Bellard
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

#ifndef AVFORMAT_PACKET_LIST_FUNCS_H
#define AVFORMAT_PACKET_LIST_FUNCS_H

#include "libavutil/error.h"
#include "packet_list.h"

int ff_packet_list_get(PacketList *pkt_buffer,
                       AVPacket      *pkt)
{
    PacketListEntry *pktl = pkt_buffer->head;
    if (!pktl)
        return AVERROR(EAGAIN);
    pkt_buffer->head = NEXT_ENTRY(pktl);
    if (!pkt_buffer->head)
        pkt_buffer->tail = NULL;
    /* Ensure pkt->opaque is blank. */
    ff_packet_list_entry_set_next(pktl, NULL);
    av_packet_move_ref(pkt, GET_PKT(pktl));
    ff_packet_list_entry_free(&pktl);
    return 0;
}

void ff_packet_list_free(PacketList *pkt_buf)
{
    PacketListEntry *tmp = pkt_buf->head;

    while (tmp) {
        PacketListEntry *pktl = tmp;
        tmp = NEXT_ENTRY(pktl);
        ff_packet_list_entry_free(&pktl);
    }
    pkt_buf->head = pkt_buf->tail = NULL;
}

#endif /* AVFORMAT_PACKET_LIST_FUNCS_H */
