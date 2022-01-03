/*
 * AVPacket List API functions
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

#ifndef AVCODEC_PACKET_INTERNAL_H
#define AVCODEC_PACKET_INTERNAL_H

#include "libavcodec/packet.h"

/**
 * This packet-list API reserves for itself the AVPacket's opaque
 * field as next-pointer. No other fields are reserved.
 *
 * Users that just want a packet FIFO can use ff_packet_list_put()
 * and ff_packet_list_get(); users desiring more fine-grained
 * control about the order of entries can use the below accessors
 * and functions to manipulate the lists more directly.
 */
typedef struct AVPacket PacketListEntry;

typedef struct PacketList {
    PacketListEntry *head, *tail;
} PacketList;

/* Get a pointer to the packet contained in a PacketListEntry.
 * Should be used instead of accessing the packet directly. */
#define GET_PKT(entry)     (entry)
/* Get a pointer to the pointer to the next entry.
 * Can be used to set the current entry's next pointer. */
#define NEXT_ENTRYP(entry) ((PacketListEntry**)&(entry)->opaque)
#define NEXT_ENTRY(entry)  (*NEXT_ENTRYP(entry))

static inline PacketListEntry *ff_packet_list_entry_alloc(void)
{
    return av_packet_alloc();
}

static inline void ff_packet_list_entry_free(PacketListEntry **entry)
{
    av_packet_free(entry);
}

static inline void ff_packet_list_entry_set_next(PacketListEntry *entry,
                                                 PacketListEntry *next)
{
    *NEXT_ENTRYP(entry) = next;
}

/**
 * Append an already existing PacketListEntry to the list.
 */
static inline void ff_packet_list_append_entry(PacketList *list,
                                               PacketListEntry *entry)
{
    if (list->tail)
        ff_packet_list_entry_set_next(list->tail, entry);
    else
        list->head = entry;
    list->tail  = entry;
    ff_packet_list_entry_set_next(entry, NULL);
}

/**
 * Append an AVPacket to the list.
 *
 * @param list  A PacketList
 * @param pkt   The packet being appended. The data described in it will
 *              be made reference counted if it isn't already.
 * @param copy  A callback to copy the contents of the packet to the list.
                May be null, in which case the packet's reference will be
                moved to the list.
 * @return 0 on success, negative AVERROR value on failure. On failure,
           the packet and the list are unchanged.
 */
int ff_packet_list_put(PacketList *list, AVPacket *pkt,
                       int (*copy)(AVPacket *dst, const AVPacket *src),
                       int flags);

/**
 * Remove the oldest AVPacket in the list and return it.
 *
 * @note The pkt will be overwritten completely on success. The caller
 *       owns the packet and must unref it by itself.
 *
 * @param head A pointer to a PacketList struct
 * @param pkt  Pointer to an AVPacket struct
 * @return 0 on success, and a packet is returned. AVERROR(EAGAIN) if
 *         the list was empty.
 */
int ff_packet_list_get(PacketList *list, AVPacket *pkt);

/**
 * Wipe the list and unref all the packets in it.
 */
void ff_packet_list_free(PacketList *list);

#endif /* AVFORMAT_PACKET_LIST_H */
