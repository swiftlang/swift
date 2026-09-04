//===--- ConcurrencyDebug.h - Concurrency debug ABI ----------*- C -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_CONCURRENCYDEBUG_H
#define SWIFT_RUNTIME_CONCURRENCYDEBUG_H

#include <stdint.h>

/// Identifies how the runtime stores the currently executing AsyncTask.
/// Debuggers use this to decide how to locate the current task on a thread.
///
/// These values are part of the debug ABI. Once published, a value must never
/// be reused for a different storage strategy. New strategies get a new value;
/// bump `_swift_concurrency_debug_internal_layout_version` when adding one.
enum swift_concurrency_current_task_storage_kind {
  /// The task pointer lives in C++ thread-local storage.
  SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_CXX_THREAD_LOCAL = 1,

  /// The task pointer lives in an ordinary global variable.
  SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_GLOBAL = 2,

  /// The task pointer lives in a reserved pthread TLS key.
  SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_PTHREAD_RESERVED_KEY = 3,

  /// The task pointer lives in a dynamically allocated pthread TLS key.
  SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_PTHREAD_ALLOCATED_KEY = 4,
};

/// Indicates that the concrete storage kind is published by the linked
/// platform library in `_swift_concurrency_debug_current_task_storage_kind`.
/// The remaining bits in the storage-kind byte must be zero.
#define SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_DEFERRED_MASK 0x80u

#ifdef __cplusplus
namespace swift {

enum class _concurrency_current_task_storage_kind : uint8_t {
  cxx_thread_local =
      SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_CXX_THREAD_LOCAL,
  global = SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_GLOBAL,
  pthread_reserved_key =
      SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_PTHREAD_RESERVED_KEY,
  pthread_allocated_key =
      SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_PTHREAD_ALLOCATED_KEY,
};

extern "C" {
#endif

/// The concrete current-task storage kind used by a platform library when the
/// runtime storage-kind byte has
/// `SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_DEFERRED_MASK` set.
///
/// The value must identify a concrete
/// `swift_concurrency_current_task_storage_kind`; it cannot itself be
/// deferred.

extern uint32_t _swift_concurrency_debug_current_task_storage_kind;

#ifdef __cplusplus
}
} // namespace swift
#endif

#endif // SWIFT_RUNTIME_CONCURRENCYDEBUG_H
