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

/// The namespace of a platform execution-context index. These values describe
/// storage ownership, independently of the current-task lookup mechanism.
enum swift_concurrency_debug_context_kind {
  SWIFT_CONCURRENCY_DEBUG_CONTEXT_SOFTWARE_THREAD = 1,
  SWIFT_CONCURRENCY_DEBUG_CONTEXT_HARDWARE_THREAD = 2,
};

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

  /// The task pointer lives in `_swift_concurrency_debug_global_tls_array`, an
  /// array of pointer-sized slots indexed by `swift::tls_key` (see
  /// swift/Threading/TLSKeys.h). The task is in slot
  /// `tls_key::concurrency_task`.
  SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_GLOBAL_TLS_ARRAY = 5,

  /// A platform table indexed by the selected execution context's index.
  SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_PLATFORM_INDEXED = 6,

  /// Calls the platform's C-ABI current-task helper on every query.
  SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_PLATFORM_FUNCTION = 7,

  /// Calls the platform's C-ABI slot-address helper once per execution context.
  SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_PLATFORM_ADDRESS_FUNCTION = 8,
};

/// Indicates that the concrete storage kind is published by the linked
/// platform library in `_swift_concurrency_debug_current_task_storage_kind`.
/// The remaining bits in the storage-kind byte must be zero.
#define SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_DEFERRED_FLAG 0x80u

#ifdef __cplusplus
extern "C" {
#endif

/// The concrete current-task storage kind used by a platform library when the
/// runtime storage-kind byte has
/// `SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_DEFERRED_FLAG` set.
///
/// The value must identify a concrete
/// `swift_concurrency_current_task_storage_kind`; it cannot itself be
/// deferred.

extern uint32_t _swift_concurrency_debug_current_task_storage_kind;

/// All platform storage kinds use the same platform-defined execution context:
/// the entity whose context-local storage holds the current-task pointer. This
/// is an OS/RTOS software thread or a hardware thread (logical CPU or hart).
/// See the execution-context contract in EmbeddedPlatform.h. A known single
/// hardware context may use index zero; failed identification must not do so.
/// The platform debugger layer must represent and select that context. It must
/// not substitute the last CPU on which a migratable thread ran. Swift tasks
/// may migrate between contexts; a task is not itself a storage context.
///
/// Indexed storage additionally requires an explicit context-to-index mapping.
/// Its table is described by a pointer to the first task-pointer slot, an
/// element count, and a byte stride, immutable for the lifetime of the process.
/// Indexes must be in [0, count). Slots need not be contiguous, permitting a
/// task slot within a context-local record. A null slot value means that
/// context is not currently executing a task. Raw debugger/protocol thread IDs
/// and hardware CPU IDs are not implicit indexes: the platform/debugger
/// contract must explicitly supply the mapping appropriate to its contexts.
/// Debuggers must reject an index greater than or equal to count before
/// computing or reading its slot. This is an unavailable lookup, not a null
/// task. The table's address range must also be checked for overflow.
extern void **const _swift_concurrency_debug_current_task_slots;
extern const uintptr_t _swift_concurrency_debug_current_task_slot_count;
extern const uintptr_t _swift_concurrency_debug_current_task_slot_stride;

/// The context kind expected by indexed storage. Debuggers must compare this
/// immutable value with the selected context's kind before resolving the table
/// metadata or reading any slot. Unknown kinds and mismatches are unavailable
/// lookups, not an invitation to reinterpret the index in another namespace.
extern const uint32_t _swift_concurrency_debug_current_task_context_kind;

/// These C-ABI helpers must be safe to invoke in the selected stopped context
/// without allocating, blocking, or acquiring locks. Debugger calls are opt-in:
/// they resume the target and do not preserve a whole-system snapshot.
/// Both receive the selected context's index and namespace. The platform, not
/// the debugger, decides whether it can answer that pair. Indexes need not be
/// table offsets for helper lookups, but must have the platform-agreed meaning
/// and remain stable for the context's lifetime. Unknown kinds must be rejected.
///
/// The value helper returns NULL for a supported context with no active task.
/// UINTPTR_MAX is reserved for a rejected/unavailable lookup, not a task pointer.
#define SWIFT_CONCURRENCY_DEBUG_TASK_UNAVAILABLE ((void *)UINTPTR_MAX)
extern void *_swift_concurrency_debug_getCurrentTask(uintptr_t index,
                                                    uint32_t context_kind);

/// The returned slot address must remain valid and bound to the same execution
/// context for its lifetime, including when no task is running or that context
/// migrates between CPUs. The debugger must not reuse the cached address for a
/// newly created context that reuses an old numeric ID. The slot helper returns
/// NULL when it cannot answer the pair. A supported but idle context must return
/// a valid slot address whose contents are NULL. No slot address is cached on
/// rejection. These two-argument helpers require concurrency debug ABI 5.
extern void **_swift_concurrency_debug_getCurrentTaskAddress(
    uintptr_t index, uint32_t context_kind);

#ifdef __cplusplus
}
#endif

#endif // SWIFT_RUNTIME_CONCURRENCYDEBUG_H
