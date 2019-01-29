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

#include <Foundation/Foundation.h>
#include "format.h"

static inline void
_os_log_encode_arg(os_trace_blob_t ob, os_log_fmt_cmd_t cmd, const void *data)
{
  os_trace_blob_add(ob, cmd, sizeof(os_log_fmt_cmd_s));
  os_trace_blob_add(ob, data, cmd->cmd_size);
}

bool
_os_log_encode(char buf[OS_LOG_FMT_BUF_SIZE], const char *format, va_list args,
    int saved_errno, os_trace_blob_t ob)
{
  os_log_fmt_hdr_s hdr = { };
  os_trace_blob_add(ob, &hdr, sizeof(hdr));

  const char *percent = strchr(format, '%');

  while (percent != NULL) {
    ++percent;
    if (percent[0] != '%') {
      os_log_fmt_cmd_s cmd = { };
      os_log_count_type_t widtht = T_C_NONE;
      os_log_count_type_t prect = T_C_NONE;
      os_log_fmt_cmd_flags_t flags = 0;
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

          case '*': // dynamic width
            widtht = T_C_DYNAMIC;
            // if the next character is a '.' then increment percent
            // and fallthrough to the precision handling code
            if (percent[1] != '.') {
              break;
            }
            percent++;
            // FALLTHROUGH
          case '.': // precision
            if ((percent[1]) == '*') {
              prect = T_C_DYNAMIC;
              percent++;
            } else {
              while (isdigit(percent[1])) {
                precision = 10 * precision + (percent[1] - '0');
                percent++;
              }
              if (precision > 1024) precision = 1024;
              prect = T_C_STATIC;
            }
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
                  flags |= OSLF_CMD_FLAG_PRIVATE;
                } else if (strncmp(percent + 1, "public", MIN(curr2 - percent - 1, 6)) == 0) {
                  flags |= OSLF_CMD_FLAG_PUBLIC;
                }
                percent = curr2;
                break;
              }
            }
            break;

#define encode_width() ({ \
  if (widtht == T_C_DYNAMIC) { \
    cmd.cmd_type = OSLF_CMD_TYPE_SCALAR; \
    encode(int, 0); \
  }})

// clang inconsistency: static precision counts are still marked with the
// privacy bits of the command they preceed
#define encode_precision(type) ({ \
  if (prect != T_C_NONE) { \
    cmd.cmd_type = type; \
    cmd.cmd_size = sizeof(int); \
    if (prect == T_C_STATIC && type == OSLF_CMD_TYPE_COUNT) { \
      cmd.cmd_flags = flags; \
    } else if (prect == T_C_DYNAMIC) { \
      precision = va_arg(args, int); \
    } \
    _os_log_encode_arg(ob, &cmd, &precision); \
    hdr.hdr_cmd_cnt++; \
    prect = T_C_NONE; \
  }})

// scalar data types encode their precision as a scalar
#define encode_scalar_preamble() ({ \
  encode_width(); \
  if (prect == T_C_DYNAMIC) { \
    encode_precision(OSLF_CMD_TYPE_SCALAR); \
  }})

#define encode_pointer_preamble() ({ \
  encode_width(); \
  encode_precision(OSLF_CMD_TYPE_COUNT); \
})

#define encode_nsstring(flags) ({ \
  NSString *__arg = va_arg(args, NSString *); \
  const char * _Nullable __var = __arg.UTF8String; \
  cmd.cmd_flags = flags; \
  cmd.cmd_size = sizeof(__var); \
  _os_log_encode_arg(ob, &cmd, &__var); \
  hdr.hdr_cmd_cnt++; \
})

#define encode_smallint(ty, flags) ({ \
  int __var = va_arg(args, int); \
  cmd.cmd_flags = flags; \
  cmd.cmd_size = sizeof(__var); \
  _os_log_encode_arg(ob, &cmd, &__var); \
  hdr.hdr_cmd_cnt++; \
})

#define encode(ty, flags) ({ \
  ty __var = va_arg(args, ty); \
  cmd.cmd_flags = flags; \
  cmd.cmd_size = sizeof(__var); \
  _os_log_encode_arg(ob, &cmd, &__var); \
  hdr.hdr_cmd_cnt++; \
})

          /* fixed types */
          case 'c': // char
          case 'd': // integer
          case 'i': // integer
          case 'o': // octal
          case 'u': // unsigned
          case 'x': // hex
          case 'X': // upper-hex
            encode_scalar_preamble();
            cmd.cmd_type = OSLF_CMD_TYPE_SCALAR;
            switch (type) {
              case T_CHAR: encode_smallint(char, flags); break;
              case T_SHORT: encode_smallint(short, flags); break;
              case T_INT: encode(int, flags); break;
              case T_LONG: encode(long, flags); break;
              case T_LONGLONG: encode(long long, flags); break;
              case T_SIZE: encode(size_t, flags); break;
              case T_INTMAX: encode(intmax_t, flags); break;
              case T_PTRDIFF: encode(ptrdiff_t, flags); break;
              default: return false;
            }
            done = true;
            break;

          case 'p': // emit pointers as scalars
            cmd.cmd_type = OSLF_CMD_TYPE_SCALAR;
            encode(void *, flags);
            done = true;
            break;

          case 'C': // wchar is treated like %lc
            encode_scalar_preamble();
            cmd.cmd_type = OSLF_CMD_TYPE_SCALAR;
            encode_smallint(wint_t, flags);
            done = true;
            break;

          case 'P': // pointer data
            encode_pointer_preamble();
            hdr.hdr_flags |= OSLF_HDR_FLAG_HAS_NON_SCALAR;
            cmd.cmd_type = OSLF_CMD_TYPE_DATA;
            cmd.cmd_size = sizeof(void *);
            encode(void *, flags);
            done = true;
            break;

          case 'L': // long double
            long_double = true;
            break;

          case 'a': case 'A': case 'e': case 'E': // floating types
          case 'f': case 'F': case 'g': case 'G':
            encode_scalar_preamble();
            cmd.cmd_type = OSLF_CMD_TYPE_SCALAR;
            if (long_double) {
              encode(long double, flags);
            } else {
              encode(double, flags);
            }
            done = true;
            break;

          case 's': // Strings sent from Swift as NSString objects
            encode_pointer_preamble();
            hdr.hdr_flags |= OSLF_HDR_FLAG_HAS_NON_SCALAR;
            cmd.cmd_type = OSLF_CMD_TYPE_STRING;
            encode_nsstring(flags);
            done = true;
            break;

          case '@': // CFTypeRef aka NSObject *
            // %@ does not support precision
            encode_width();
            hdr.hdr_flags |= OSLF_HDR_FLAG_HAS_NON_SCALAR;
            cmd.cmd_type = OSLF_CMD_TYPE_OBJECT;
            encode(void *, flags);
            done = true;
            break;

          case 'm':
            cmd.cmd_type = OSLF_CMD_TYPE_SCALAR;
            cmd.cmd_size = sizeof(int);
            cmd.cmd_flags = flags;
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

#undef encode_nsstring
#undef encode_smallint
#undef encode
