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
#include <dlfcn.h>
#include <dispatch/dispatch.h>
#include <os/base.h>
#include <os/log.h>
#include <objc/runtime.h>
#include <wchar.h>

#include "os_trace_blob.h"
#include "thunks.h"

#define OST_FORMAT_MAX_STRING_SIZE 1024
#define OS_LOG_FMT_MAX_CMDS    48
#define OS_LOG_FMT_BUF_SIZE    (2 + (2 + 16) * OS_LOG_FMT_MAX_CMDS)

enum os_trace_int_types_t {
  T_CHAR = -2,
  T_SHORT = -1,
  T_INT = 0,
  T_LONG = 1,
  T_LONGLONG = 2,
  T_SIZE = 3,
  T_INTMAX = 4,
  T_PTRDIFF = 5,
};

OS_ENUM(os_log_fmt_cmd_flags, uint8_t,
    OSLF_CMD_FLAG_PRIVATE = 0x1,
    OSLF_CMD_FLAG_PUBLIC  = 0x2,
);

OS_ENUM(os_log_fmt_cmd_type, uint8_t,
    OSLF_CMD_TYPE_SCALAR      = 0,
    OSLF_CMD_TYPE_COUNT       = 1,
    OSLF_CMD_TYPE_STRING      = 2,
    OSLF_CMD_TYPE_DATA        = 3,
    OSLF_CMD_TYPE_OBJECT      = 4,
    OSLF_CMD_TYPE_WIDE_STRING = 5,
    OSLF_CMD_TYPE_ERRNO       = 6,
);

OS_ENUM(os_log_fmt_hdr_flags, uint8_t,
    OSLF_HDR_FLAG_HAS_PRIVATE    = 0x01,
    OSLF_HDR_FLAG_HAS_NON_SCALAR = 0x02,
);

enum os_log_int_types_t {
  OST_CHAR = -2,
  OST_SHORT = -1,
  OST_INT =  0,
  OST_LONG =  1,
  OST_LONGLONG =  2,
  OST_SIZE =  3,
  OST_INTMAX =  4,
  OST_PTRDIFF =  5,
};

typedef struct {
    os_log_fmt_cmd_flags_t cmd_flags : 4;
    os_log_fmt_cmd_type_t cmd_type : 4;
    uint8_t cmd_size;
    uint8_t cmd_data[];
} os_log_fmt_cmd_s, *os_log_fmt_cmd_t;

typedef struct os_log_fmt_hdr_s {
    os_log_fmt_hdr_flags_t hdr_flags;
    uint8_t hdr_cmd_cnt;
    uint8_t hdr_data[];
} os_log_fmt_hdr_s, *os_log_fmt_hdr_t;

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

API_AVAILABLE(macosx(10.12.4), ios(10.3), tvos(10.2), watchos(3.2))
void
os_log_pack_send(os_log_pack_t pack, os_log_t log, os_log_type_t type);

static inline void
_os_log_encode_arg(os_trace_blob_t ob, os_log_fmt_cmd_t cmd, const void *data)
{
  os_trace_blob_add(ob, cmd, sizeof(os_log_fmt_cmd_s));
  os_trace_blob_add(ob, data, cmd->cmd_size);
}

static bool
_os_log_encode(char buf[OS_LOG_FMT_BUF_SIZE], const char *format, va_list args, int saved_errno, os_trace_blob_t ob)
{
  os_log_fmt_hdr_s hdr = { };
  os_trace_blob_add(ob, &hdr, sizeof(hdr));

  const char *percent = strchr(format, '%');

  while (percent != NULL) {
    ++percent;
    if (percent[0] != '%') {
      os_log_fmt_cmd_s cmd = { };
      int type = T_INT;
      bool long_double = false;
      int precision = 0;
      char ch;

      if (hdr.hdr_cmd_cnt == OS_LOG_FMT_MAX_CMDS) {
        break;
      }

      for (bool done = false; !done; percent++) {
        switch (ch = percent[0]) {
          /* type of types or other */
          case 'l': type++; break; // longer
          case 'h': type--; break; // shorter
          case 'z': type = T_SIZE; break;
          case 'j': type = T_INTMAX; break;
          case 't': type = T_PTRDIFF; break;

          case '.': // precision
            cmd.cmd_type = OSLF_CMD_TYPE_COUNT;
            cmd.cmd_size = sizeof(int);

            if ((percent[1]) == '*') {
              precision = va_arg(args, int);
              percent++;
            } else {
              while (isdigit(percent[1])) {
                precision = 10 * precision + (percent[1] - '0');
                percent++;
              }
              if (precision > 1024) precision = 1024;
            }
            _os_log_encode_arg(ob, &cmd, &precision);
            hdr.hdr_cmd_cnt++;
            break;

          case '-': // left-align
          case '+': // force sign
          case ' ': // prefix non-negative with space
          case '#': // alternate
          case '\'': // group by thousands
            break;

          case '{': // annotated symbols
            for (const char *curr2 = percent + 1; (ch = (*curr2)) != 0; curr2++) {
              if (ch == '}') {
                if (strncmp(percent + 1, "private", MIN(curr2 - percent - 1, 7)) == 0) {
                  hdr.hdr_flags |= OSLF_HDR_FLAG_HAS_PRIVATE;
                  cmd.cmd_flags |= OSLF_CMD_FLAG_PRIVATE;
                } else if (strncmp(percent + 1, "public", MIN(curr2 - percent - 1, 6)) == 0) {
                  cmd.cmd_flags |= OSLF_CMD_FLAG_PUBLIC;
                }
                percent = curr2;
                break;
              }
            }
            break;

#define encode_smallint(ty) ({ \
  int __var = va_arg(args, int); \
  cmd.cmd_size = sizeof(__var); \
  _os_log_encode_arg(ob, &cmd, &__var); \
  hdr.hdr_cmd_cnt++; })

#define encode(ty) ({ \
  ty __var = va_arg(args, ty); \
  cmd.cmd_size = sizeof(__var); \
  _os_log_encode_arg(ob, &cmd, &__var); \
  hdr.hdr_cmd_cnt++; })

          /* fixed types */
          case 'c': // char
          case 'd': // integer
          case 'i': // integer
          case 'o': // octal
          case 'u': // unsigned
          case 'x': // hex
          case 'X': // upper-hex
            cmd.cmd_type = OSLF_CMD_TYPE_SCALAR;
            switch (type) {
              case T_CHAR: encode_smallint(char); break;
              case T_SHORT: encode_smallint(short); break;
              case T_INT: encode(int); break;
              case T_LONG: encode(long); break;
              case T_LONGLONG: encode(long long); break;
              case T_SIZE: encode(size_t); break;
              case T_INTMAX: encode(intmax_t); break;
              case T_PTRDIFF: encode(ptrdiff_t); break;
              default: return false;
            }
            done = true;
            break;

          case 'P': // pointer data
            if (precision > 0) { // only encode a pointer if we have been given a length
              hdr.hdr_flags |= OSLF_HDR_FLAG_HAS_NON_SCALAR;
              cmd.cmd_type = OSLF_CMD_TYPE_DATA;
              cmd.cmd_size = sizeof(void *);
              void *p = va_arg(args, void *);
              _os_log_encode_arg(ob, &cmd, &p);
              hdr.hdr_cmd_cnt++;
              precision = 0;
              done = true;
            }
            break;

          case 'L': // long double
            long_double = true;
            break;

          case 'a': case 'A': case 'e': case 'E': // floating types
          case 'f': case 'F': case 'g': case 'G':
            cmd.cmd_type = OSLF_CMD_TYPE_SCALAR;
            if (long_double) {
              encode(long double);
            } else {
              encode(double);
            }
            done = true;
            break;

#if 0
          case 'C': // wide-char
            value.type.wch = va_arg(args, wint_t);
            _os_log_encode_arg(&value.type.wch, sizeof(value.type.wch), OS_LOG_BUFFER_VALUE_TYPE_SCALAR, flags, context);
            done = true;
            break;
#endif

#if 0
          // String types get sent from Swift as NSString objects.
          case 's': // string
            value.type.pch = va_arg(args, char *);
            context->buffer->flags |= OS_LOG_BUFFER_HAS_NON_SCALAR;
            _os_log_encode_arg(&value.type.pch, sizeof(value.type.pch), OS_LOG_BUFFER_VALUE_TYPE_STRING, flags, context);
            prec = 0;
            done = true;
            break;
#endif

          case '@': // CFTypeRef aka NSObject *
            hdr.hdr_flags |= OSLF_HDR_FLAG_HAS_NON_SCALAR;
            cmd.cmd_type = OSLF_CMD_TYPE_OBJECT;
            encode(void *);
            done = true;
            break;

          case 'm':
            cmd.cmd_type = OSLF_CMD_TYPE_SCALAR;
            cmd.cmd_size = sizeof(int);
            _os_log_encode_arg(ob, &cmd, &saved_errno);
            hdr.hdr_cmd_cnt++;
            done = true;
            break;

          default:
            if (isdigit(ch)) { // [0-9]
              continue;
            }
            done = true;
            break;
        }

        if (done) {
          percent = strchr(percent, '%'); // Find next format
          break;
        }
      }
    } else {
      percent = strchr(percent+1, '%'); // Find next format after %%
    }
  }
  *(os_log_fmt_hdr_t)buf = hdr;
  return true;
}

#undef encode_smallint
#undef encode

__attribute__((__visibility__("default")))
void
_swift_os_log(void *dso, void *retaddr, os_log_t oslog, os_log_type_t type, const char *format, va_list args)
{
  int saved_errno = errno; // %m
  char buf[OS_LOG_FMT_BUF_SIZE];
  os_trace_blob_s ob = {
    .ob_s = buf,
    .ob_size = OS_LOG_FMT_BUF_SIZE,
    .ob_binary = true
  };

  if (_os_log_encode(buf, format, args, saved_errno, &ob)) {
    // Use os_log_pack_send where available.
    if (os_log_pack_send) {
      size_t sz = _os_log_pack_size(ob.ob_len);
      union { os_log_pack_s pack; uint8_t buf[OS_LOG_FMT_BUF_SIZE + sizeof(os_log_pack_s)]; } u;
      // _os_log_encode has already packed `saved_errno` into a OSLF_CMD_TYPE_SCALAR command
      // as the OSLF_CMD_TYPE_ERRNO does not deploy backwards, so passes zero for errno here.
      uint8_t *ptr = _os_log_pack_fill(&u.pack, sz, 0, dso, format);
      u.pack.olp_pc = retaddr;
      memcpy(ptr, buf, ob.ob_len);
      os_log_pack_send(&u.pack, oslog, type);
    } else {
      _os_log_impl(dso, oslog, type, format, (uint8_t *)buf, ob.ob_len);
    }
  }
}

__attribute__((__visibility__("default")))
void *
_swift_os_log_return_address(void)
{
  return __builtin_return_address(1);
}

