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

enum swift_concurrency_current_task_storage_kind {
  SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_CXX_THREAD_LOCAL = 1,
  SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_GLOBAL = 2,
  SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_PTHREAD_RESERVED_KEY = 3,
  SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_PTHREAD_ALLOCATED_KEY = 4,
  SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_PLATFORM_DEFINED = 5,
};

/// The concrete current-task storage kind used by a platform library when the
/// Concurrency runtime reports `platform_defined`.
///
/// The value must identify a concrete
/// `swift_concurrency_current_task_storage_kind`; it cannot itself be
/// `SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_PLATFORM_DEFINED`.
#ifdef __cplusplus
namespace swift {
extern "C" {
#endif

extern uint32_t _swift_concurrency_debug_current_task_storage_kind;

#ifdef __cplusplus
}
} // namespace swift
#endif

#endif // SWIFT_RUNTIME_CONCURRENCYDEBUG_H
