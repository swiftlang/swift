//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <ctype.h>
#include <dispatch/dispatch.h>
#include <os/base.h>
#include <os/log.h>
#include <locale.h>
#if !defined(__linux__)
#include <xlocale.h>
#endif
#include "os_trace_blob.h"

OS_NOINLINE
static void
_os_trace_temporary_resource_shortage(void)
{
    sleep(1);
}

static void *
_os_trace_malloc(size_t size)
{
    void *buf;
    while (os_unlikely(!(buf = malloc(size)))) {
        _os_trace_temporary_resource_shortage();
    }
    return buf;
}

static void *
_os_trace_memdup(void *ptr, size_t size)
{
    return memcpy(_os_trace_malloc(size), ptr, size);
}

static void *
_os_trace_realloc(void *ptr, size_t size)
{
    void *buf;
    while (os_unlikely(!(buf = realloc(ptr, size)))) {
        _os_trace_temporary_resource_shortage();
    }
    return buf;
}

void
os_trace_blob_destroy_slow(os_trace_blob_t ob)
{
    char *s = ob->ob_s;
    ob->ob_s = (void *)0xEBADF000;
    ob->ob_flags = 0;
    free(s);
}

char *
os_trace_blob_detach(os_trace_blob_t ob, size_t *len)
{
    bool needs_free = ob->ob_flags & OS_TRACE_BLOB_NEEDS_FREE;
    char *s = ob->ob_s;
    ob->ob_s = (void *)0xEBADF000;
    ob->ob_flags = 0;
    if (len) *len = ob->ob_len;
    if (needs_free) return s;
    return _os_trace_memdup(s, ob->ob_len + !ob->ob_binary);
}

OS_NOINLINE
static uint32_t
os_trace_blob_grow(os_trace_blob_t ob, size_t hint)
{
    uint32_t size, minsize, used = ob->ob_len + !ob->ob_binary;
    if (os_add_overflow(used, hint, &minsize)) {
        size = ob->ob_maxsize;
    } else if (os_mul_overflow(ob->ob_size, 2, &size)) {
        size = ob->ob_maxsize;
    } else {
        size = MIN((uint32_t)ob->ob_maxsize, MAX(minsize, size));
    }
    if (size > ob->ob_size) {
        if (ob->ob_flags & OS_TRACE_BLOB_NEEDS_FREE) {
            ob->ob_s = _os_trace_realloc(ob->ob_s, size);
        } else {
            char *s = ob->ob_s;
            ob->ob_s = _os_trace_malloc(size);
            memcpy(ob->ob_s, s, used);
            ob->ob_flags |= OS_TRACE_BLOB_NEEDS_FREE;
        }
        ob->ob_size = size;
    }
    return size - used;
}

uint32_t
os_trace_blob_add_slow(os_trace_blob_t ob, const void *ptr, size_t size)
{
    if (os_unlikely(ob->ob_flags & OS_TRACE_BLOB_TRUNCATED)) {
        return 0;
    }

    uint32_t avail = _os_trace_blob_available(ob);
    if (avail < size) {
        if (ob->ob_size < ob->ob_maxsize) {
            avail = os_trace_blob_grow(ob, size);
        }
        if (avail < size) {
            ob->ob_flags |= OS_TRACE_BLOB_TRUNCATED;
            size = avail;
        }
    }

    memcpy(ob->ob_s + ob->ob_len, ptr, size);
    return _os_trace_blob_growlen(ob, size);
}
