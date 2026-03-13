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

/// The current version of internal data structures that lldb may decode.
/// The version numbers used so far are:
/// 1 - The initial version number when this variable was added, in swift 6.1.
SWIFT_EXPORT_FROM(swift_Concurrency)
uint32_t _swift_concurrency_debug_internal_layout_version;

} // namespace swift

#endif
