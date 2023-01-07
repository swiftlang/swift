//===--- VoucherShims.h - Shims for OS vouchers --------------------*- C++ -*-//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Shims for interfacing with OS voucher calls.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CONCURRENCY_VOUCHERSHIMS_H
#define SWIFT_CONCURRENCY_VOUCHERSHIMS_H

#include "Config.h"

// swift-corelibs-libdispatch has os/voucher_private.h but it doesn't work for
// us yet, so only look for it on Apple platforms.  We also don't need vouchers
// in the single threaded concurrency runtime.
#if __APPLE__ && !SWIFT_THREADING_NONE && __has_include(<os/voucher_private.h>)
#define SWIFT_HAS_VOUCHER_HEADER 1
#include <os/voucher_private.h>
#endif

// A "dead" voucher pointer, indicating that a voucher has been removed from
// a Job, distinct from a NULL voucher that could just mean no voucher was
// present. This allows us to catch problems like adopting a voucher from the
// same Job twice without restoring it.
#define SWIFT_DEAD_VOUCHER ((voucher_t)-1)

// The OS has voucher support if it has the header or if it has ObjC interop.
#if (SWIFT_HAS_VOUCHER_HEADER || SWIFT_OBJC_INTEROP) && !SWIFT_THREADING_NONE
#define SWIFT_HAS_VOUCHERS 1
#endif

#if SWIFT_HAS_VOUCHERS

#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
#error Cannot use task-to-thread model with vouchers
#endif

#if SWIFT_HAS_VOUCHER_HEADER

#else

// If the header isn't available, declare the necessary calls here.

#include <os/object.h>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wgnu-zero-variadic-macro-arguments"
OS_OBJECT_DECL_CLASS(voucher);
#pragma clang diagnostic pop

extern "C" voucher_t _Nullable voucher_copy(void);

// Consumes argument, returns retained value.
extern "C" voucher_t _Nullable voucher_adopt(voucher_t _Nullable voucher);

#endif // __has_include(<os/voucher_private.h>)

static inline void swift_voucher_release(voucher_t _Nullable voucher) {
  // This NULL check isn't necessary, but NULL vouchers will be common, so
  // optimize for that.
  if (!voucher)
    return;
  if (voucher == SWIFT_DEAD_VOUCHER)
    return;
  os_release(voucher);
}

#else  // __APPLE__

// Declare some do-nothing stubs for OSes without voucher support.
typedef void *voucher_t;
static inline voucher_t _Nullable voucher_copy(void) { return nullptr; }
static inline voucher_t _Nullable voucher_adopt(voucher_t _Nullable voucher) {
  return nullptr;
}
static inline void swift_voucher_release(voucher_t _Nullable voucher) {}
#endif // __APPLE__

// Declare our own voucher_needs_adopt for when we don't get it from the SDK.
// This declaration deliberately takes `void *` instead of `voucher_t`. When the
// SDK provides one that takes `voucher_t`, then C++ overload resolution will
// favor that one. When the SDK does not provide a declaration, then the call
// site will invoke this stub instead.
static inline bool voucher_needs_adopt(void * _Nullable voucher) {
  return true;
}

static inline bool swift_voucher_needs_adopt(voucher_t _Nullable voucher) {
#if __APPLE__
  if (__builtin_available(macOS 12.0, iOS 15.0, tvOS 15.0, watchOS 8.0, *))
    return voucher_needs_adopt(voucher);
  return true;
#else
  return voucher_needs_adopt(voucher);
#endif
}

#endif // SWIFT_CONCURRENCY_VOUCHERSHIMS_H
