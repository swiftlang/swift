//===--- DispatchShims.h - Shims for dispatch vended APIs --------------------*- C++ -*-//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CONCURRENCY_DISPATCHSHIMS_H
#define SWIFT_CONCURRENCY_DISPATCHSHIMS_H

#include "Concurrency.h"

#if SWIFT_CONCURRENCY_ENABLE_PRIORITY_ESCALATION
#include <dispatch/swift_concurrency_private.h>

#if SWIFT_CONCURRENCY_TASK_TO_THREAD_MODEL
#error Cannot use task-to-thread model with priority escalation
#endif

// This macro is defined in newer versions of
// dispatch/swift_concurrency_private.h to indicate which version of the
// interface we are compiling with. This allows us to compile out calls to
// functions that only have decalarations on newer SDKs when compiling with
// older SDKs regardless of the availability of the functions themselves.
#ifndef DISPATCH_SWIFT_CONCURRENCY_PRIVATE_INTERFACE_VERSION
#define DISPATCH_SWIFT_CONCURRENCY_PRIVATE_INTERFACE_VERSION 0
#endif

// Provide wrappers with runtime checks to make sure that the dispatch functions
// are only called on OS-es where they are supported
static inline dispatch_thread_override_info_s
swift_dispatch_thread_get_current_override_qos_floor()
{
  if (__builtin_available(macOS 13.0, iOS 16.0, tvOS 16.0, watchOS 9.0, *)) {
    return dispatch_thread_get_current_override_qos_floor();
  }

  return (dispatch_thread_override_info_s){
      0,                     // can_override
      0,                     // unused
      QOS_CLASS_UNSPECIFIED, // override_qos_floor
  };
}

static inline int
swift_dispatch_thread_override_self(qos_class_t override_qos) {

  if (__builtin_available(macOS 13.0, iOS 16.0, tvOS 16.0, watchOS 9.0, *)) {
    return dispatch_thread_override_self(override_qos);
  }

  return 0;
}

static inline uint32_t
swift_dispatch_thread_override_self_with_base(qos_class_t override_qos, qos_class_t base_qos) {

#if DISPATCH_SWIFT_CONCURRENCY_PRIVATE_INTERFACE_VERSION >= 1
  if (__builtin_available(macOS 9998, iOS 9998, tvOS 9998, watchOS 9998, *)) {
    return dispatch_thread_override_self_with_base(override_qos, base_qos);
  } else
#endif
  if (__builtin_available(macOS 13.0, iOS 16.0, tvOS 16.0, watchOS 9.0, *)) {
    // If we don't have the ability to set our base qos correctly, at least set the override
    // We want to return 0 here because we have nothing to reset in this case
    (void) dispatch_thread_override_self(override_qos);
  }

  return 0;
}

static inline void
swift_dispatch_thread_reset_override_self(uint32_t opaque) {

#if DISPATCH_SWIFT_CONCURRENCY_PRIVATE_INTERFACE_VERSION >= 1
  if (__builtin_available(macOS 9998, iOS 9998, tvOS 9998, watchOS 9998, *)) {
    dispatch_thread_reset_override_self(opaque);
  }
#endif
}

static inline int
swift_dispatch_lock_override_start_with_debounce(dispatch_lock_t *lock_addr,
   dispatch_tid_t expected_thread, qos_class_t override_to_apply) {

  if (__builtin_available(macOS 13.0, iOS 16.0, tvOS 16.0, watchOS 9.0, *)) {
    return dispatch_lock_override_start_with_debounce(lock_addr, expected_thread, override_to_apply);
  }

  return 0;
}

static inline int
swift_dispatch_lock_override_end(qos_class_t override_to_end) {
  if (__builtin_available(macOS 13.0, iOS 16.0, tvOS 16.0, watchOS 9.0, *)) {
    return dispatch_lock_override_end(override_to_end);
  }

  return 0;
}
#endif

#endif /* SWIFT_CONCURRENCY_DISPATCHSHIMS_H */
