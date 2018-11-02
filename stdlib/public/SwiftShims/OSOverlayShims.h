//===--- OSOverlayShims.h ---------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_STDLIB_SHIMS_OS_OVERLAY_H
#define SWIFT_STDLIB_SHIMS_OS_OVERLAY_H

#include <os/log.h>
#include <os/signpost.h>
#include <stdarg.h>
#include "Visibility.h"

static inline os_log_t _Nonnull
_swift_os_log_default(void) {
  return OS_LOG_DEFAULT;
}

static inline os_log_t _Nonnull
_swift_os_log_disabled(void) {
  return OS_LOG_DISABLED;
}

static inline const unsigned char * _Nonnull
_swift_os_signpost_points_of_interest(void) {
  /* OS_LOG_CATEGORY_POINTS_OF_INTEREST */
  return "PointsOfInterest";
}

static inline os_signpost_id_t
_swift_os_signpost_id_exclusive(void) {
  /* OS_SIGNPOST_ID_EXCLUSIVE */
  return (os_signpost_id_t)0xEEEEB0B5B2B2EEEE;
}

static inline os_signpost_id_t
_swift_os_signpost_id_invalid(void) {
  /* OS_SIGNPOST_ID_INVALID */
  return (os_signpost_id_t)~0;
}

static inline os_signpost_id_t
_swift_os_signpost_id_null(void) {
  /* OS_SIGNPOST_ID_NULL */
  return (os_signpost_id_t)0;
}

SWIFT_RUNTIME_STDLIB_INTERNAL
extern const void * _Nullable
_swift_os_log_return_address(void);

SWIFT_RUNTIME_STDLIB_INTERNAL
extern void
_swift_os_log(
    const void * _Nullable dso,
    const void * _Nullable ra,
    os_log_t _Nonnull h,
    os_log_type_t type,
    const char * _Nonnull fmt,
    va_list args);

SWIFT_RUNTIME_STDLIB_INTERNAL
extern void
_swift_os_signpost_with_format(
    const void * _Nullable dso,
    const void * _Nullable ra,
    os_log_t _Nonnull h,
    os_signpost_type_t spty,
    const char * _Nonnull spnm,
    os_signpost_id_t spid,
    const char * _Nullable fmt,
    va_list args);

SWIFT_RUNTIME_STDLIB_INTERNAL
extern void
_swift_os_signpost(
    const void * _Nullable dso,
    const void * _Nullable ra,
    os_log_t _Nonnull h,
    os_signpost_type_t spty,
    const char * _Nonnull spnm,
    os_signpost_id_t spid);

#endif // SWIFT_STDLIB_SHIMS_OS_OVERLAY_H
