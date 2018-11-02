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

#include <TargetConditionals.h>
#include <Availability.h>
#include <CoreFoundation/CoreFoundation.h>
#include <Foundation/Foundation.h>
#include <dlfcn.h>
#include <dispatch/dispatch.h>
#include <os/base.h>
#include <os/log.h>
#include <os/signpost.h>
#include <objc/runtime.h>
#include <wchar.h>

#include "thunks.h"
#include "format.h"

#define OST_FORMAT_MAX_STRING_SIZE 1024

typedef struct os_log_pack_s {
    uint64_t        olp_continuous_time;
    struct timespec olp_wall_time;
    const void     *olp_mh;
    const void     *olp_pc;
    const char     *olp_format;
    uint8_t         olp_data[0];
} os_log_pack_s, *os_log_pack_t;

API_AVAILABLE(macosx(10.12.4), ios(10.3), tvos(10.2), watchos(3.2))
size_t
_os_log_pack_size(size_t os_log_format_buffer_size);

API_AVAILABLE(macosx(10.12.4), ios(10.3), tvos(10.2), watchos(3.2))
uint8_t *
_os_log_pack_fill(os_log_pack_t pack, size_t size, int saved_errno, const void *dso, const char *fmt);

API_AVAILABLE(macosx(10.14), ios(12.0), tvos(12.0), watchos(5.0))
uint8_t *
_os_signpost_pack_fill(os_log_pack_t pack, size_t size,
        int saved_errno, const void *dso, const char *fmt,
        const char *spnm, os_signpost_id_t spid);

API_AVAILABLE(macosx(10.12.4), ios(10.3), tvos(10.2), watchos(3.2))
void
os_log_pack_send(os_log_pack_t pack, os_log_t log, os_log_type_t type);

API_AVAILABLE(macosx(10.14), ios(12.0), tvos(12.0), watchos(5.0))
void
_os_signpost_pack_send(os_log_pack_t pack, os_log_t h,
        os_signpost_type_t spty);

__attribute__((__visibility__("default")))
void *
_swift_os_log_return_address(void)
{
  return __builtin_return_address(1);
}

__attribute__((__visibility__("default")))
void
_swift_os_log(
    const void * _Nullable dso,
    const void * _Nullable ra,
    os_log_t _Nonnull h,
    os_log_type_t type,
    const char * _Nonnull fmt,
    va_list args)
{
  int saved_errno = errno; // %m
  char buf[OS_LOG_FMT_BUF_SIZE];
  os_trace_blob_s ob = {
    .ob_s = buf,
    .ob_size = OS_LOG_FMT_BUF_SIZE,
    .ob_binary = true
  };

  if (_os_log_encode(buf, fmt, args, saved_errno, &ob)) {
    if (os_log_pack_send) {
      size_t sz = _os_log_pack_size(ob.ob_len);
      union {
        os_log_pack_s pack;
        uint8_t buf[OS_LOG_FMT_BUF_SIZE + sizeof(os_log_pack_s)];
      } u;
      /*
       * _os_log_encode has already packed `saved_errno` into a
       * OSLF_CMD_TYPE_SCALAR command as the OSLF_CMD_TYPE_ERRNO does not
       * deploy backwards, so pass zero for errno here.
       */
      uint8_t *ptr = _os_log_pack_fill(&u.pack, sz, 0, dso, fmt);
      u.pack.olp_pc = ra;
      memcpy(ptr, buf, ob.ob_len);
      os_log_pack_send(&u.pack, h, type);
    } else {
      _os_log_impl((void *)dso, h, type, fmt, (uint8_t *)buf, ob.ob_len);
    }
  }
}

API_AVAILABLE(macosx(10.14), ios(12.0), tvos(12.0), watchos(5.0))
__attribute__((__visibility__("default")))
void
_swift_os_signpost_with_format(
    const void * _Nullable dso,
    const void * _Nullable ra,
    os_log_t _Nonnull h,
    os_signpost_type_t spty,
    const char * _Nonnull spnm,
    os_signpost_id_t spid,
    const char * _Nullable fmt,
    va_list args)
{
  int saved_errno = errno; // %m
  char buf[OS_LOG_FMT_BUF_SIZE];
  os_trace_blob_s ob = {
    .ob_s = buf,
    .ob_size = OS_LOG_FMT_BUF_SIZE,
    .ob_maxsize = OS_LOG_FMT_BUF_SIZE,
    .ob_binary = true
  };

  /*
   * Signposts with a signpost name but no message/arguments are valid;
   * the underlying encode/pack/decode infrastructure agrees to treat
   * these like having an empty format string.
   */
  bool encoded = fmt == NULL ?
      _os_log_encode(buf, "", args, saved_errno, &ob) :
      _os_log_encode(buf, fmt, args, saved_errno, &ob);
  if (encoded) {
    size_t sz = _os_log_pack_size(ob.ob_len);
    union {
      os_log_pack_s pack;
      uint8_t buf[OS_LOG_FMT_BUF_SIZE + sizeof(os_log_pack_s)];
    } u;
    uint8_t *ptr = _os_signpost_pack_fill(&u.pack, sz, saved_errno, dso,
        fmt, spnm, spid);
    u.pack.olp_pc = ra;
    memcpy(ptr, buf, ob.ob_len);
    _os_signpost_pack_send(&u.pack, h, spty);
  }
}

API_AVAILABLE(macosx(10.14), ios(12.0), tvos(12.0), watchos(5.0))
__attribute__((__visibility__("default")))
void
_swift_os_signpost(
    const void * _Nullable dso,
    const void * _Nullable ra,
    os_log_t _Nonnull h,
    os_signpost_type_t spty,
    const char * _Nonnull spnm,
    os_signpost_id_t spid)
{
  static va_list x;
  _swift_os_signpost_with_format(dso, ra, h, spty, spnm, spid, NULL, x);
}
