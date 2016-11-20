//===--- OSOverlayShims.h ---------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_OS_OVERLAY_H
#define SWIFT_STDLIB_SHIMS_OS_OVERLAY_H

#include <os/log.h>
#include <stdarg.h>

extern void _swift_os_log(const void * _Nullable dso, os_log_t _Nonnull oslog,
                          os_log_type_t type, const char * _Nonnull format,
                          va_list args);

static inline os_log_t _Nonnull
_swift_os_log_default(void) {
  return OS_LOG_DEFAULT;
}

#endif // SWIFT_STDLIB_SHIMS_OS_OVERLAY_H

