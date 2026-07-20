//===--- Debug.h - Swift Concurrency debug helpers --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Debugging and inspection support.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CONCURRENCY_DEBUG_H
#define SWIFT_CONCURRENCY_DEBUG_H

#include <cstdint>

#include "swift/Runtime/Config.h"

namespace swift {

// Dispatch knows about these symbol names. Don't change them without consulting
// dispatch.

/// The metadata pointer used for job objects.
SWIFT_EXPORT_FROM(swift_Concurrency)
const void *const _swift_concurrency_debug_jobMetadata;

/// The metadata pointer used for async task objects.
SWIFT_EXPORT_FROM(swift_Concurrency)
const void *const _swift_concurrency_debug_asyncTaskMetadata;

/// The size of an AsyncTask, in bytes.
SWIFT_EXPORT_FROM(swift_Concurrency)
const size_t _swift_concurrency_debug_asyncTaskSize;

/// Offset from the start of an `AsyncTask` to its `NameFragment`
/// (a single `const char *` pointing to the task's null-terminated name).
///
/// The fragment is ONLY present when `JobFlags::task_hasInitialTaskName()` is true.
SWIFT_EXPORT_FROM(swift_Concurrency)
const size_t _swift_concurrency_debug_asyncTaskNameOffset;

/// A fake metadata pointer placed at the start of async task slab allocations.
SWIFT_EXPORT_FROM(swift_Concurrency)
const void *const _swift_concurrency_debug_asyncTaskSlabMetadata;

SWIFT_EXPORT_FROM(swift_Concurrency)
const void *const _swift_concurrency_debug_non_future_adapter;
SWIFT_EXPORT_FROM(swift_Concurrency)
const void *const _swift_concurrency_debug_future_adapter;
SWIFT_EXPORT_FROM(swift_Concurrency)
const void *const _swift_concurrency_debug_task_wait_throwing_resume_adapter;
SWIFT_EXPORT_FROM(swift_Concurrency)
const void *const _swift_concurrency_debug_task_future_wait_resume_adapter;

/// Whether the runtime we are inspecting supports priority escalation
SWIFT_EXPORT_FROM(swift_Concurrency)
bool _swift_concurrency_debug_supportsPriorityEscalation;

/// Identifies how the runtime stores the currently executing AsyncTask.
/// Debuggers use this to decide how to locate the current task on a thread.
///
/// The values are part of the debug ABI — once published, a value must
/// never be reused for a different storage strategy. New strategies get a
/// new value; bump _swift_concurrency_debug_internal_layout_version when
/// adding one.
enum class _concurrency_current_task_storage_kind : uint8_t {
  /// The task pointer lives in TLS at offset 0 of the runtime's internal
  /// current-task variable, which the debugger locates via debug info.
  cxx_thread_local = 1,

  /// The task pointer lives at offset 0 of the runtime's internal current-task
  /// variable (no TLS resolution). Used by single-threaded /
  /// SWIFT_THREADING_NONE builds where `SWIFT_THREAD_LOCAL` expands to nothing.
  global = 2,

  /// Pthread thread-specific data with a *reserved* (compile-time constant)
  /// key. Used on Darwin. The key value should be inferred by the debugger.
  pthread_reserved_key = 3,

  /// Pthread thread-specific data with a *dynamically-allocated* key. Support
  /// for this may require writing the key value in a yet-to-be-defined global
  /// variable.
  pthread_allocated_key = 4,
};

/// The current version of internal data structures that lldb may decode.
/// The version numbers used so far are:
/// 1 - The initial version number when this variable was added, in swift 6.1.
/// 2 - Task names moved from a record to a dedicated fragment,
///     tail allocated just after the AsyncTask itself.
/// 3 - The top 8 bits of this value have been reserved to expose how runtimes
///     store the current task (_concurrency_current_task_storage_kind).
SWIFT_EXPORT_FROM(swift_Concurrency)
uint32_t _swift_concurrency_debug_internal_layout_version;

} // namespace swift

#endif
