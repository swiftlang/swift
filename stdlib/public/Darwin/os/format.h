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

#ifndef __OS_FORMAT_H__
#define __OS_FORMAT_H__

#include <TargetConditionals.h>
#include <Availability.h>
#include <os/base.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>

#include "os_trace_blob.h"

#define OS_LOG_FMT_MAX_CMDS    48
#define OS_LOG_FMT_BUF_SIZE    (2 + (2 + 16) * OS_LOG_FMT_MAX_CMDS)

typedef enum {
  T_C_NONE = 0,
  T_C_STATIC = 1,
  T_C_DYNAMIC = 2,
} os_log_count_type_t;

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

__attribute__((__visibility__("hidden")))
bool
_os_log_encode(char buf[OS_LOG_FMT_BUF_SIZE], const char *format, va_list args,
    int saved_errno, os_trace_blob_t ob);

#endif // __OS_FORMAT_H__
