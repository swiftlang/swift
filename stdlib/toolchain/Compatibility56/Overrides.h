//===--- Overrides.h - Compat overrides for Swift 5.6 runtime ------s------===//
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
//  This file provides compatibility override hooks for Swift 5.6 runtimes.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Metadata.h"
#include "llvm/ADT/StringRef.h"
#include "CompatibilityOverride.h"

namespace swift {
struct OpaqueValue;
class AsyncContext;
class AsyncTask;

using TaskFutureWait_t = SWIFT_CC(swiftasync) void(
                              OpaqueValue *result,
                              SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
                              AsyncTask *task,
                              TaskContinuationFunction *resumeFn,
                              AsyncContext *callContext);

using TaskFutureWaitThrowing_t = SWIFT_CC(swiftasync) void(
                              OpaqueValue *result,
                              SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
                              AsyncTask *task,
                              ThrowingTaskFutureWaitContinuationFunction *resumeFn,
                              AsyncContext *callContext);

__attribute__((weak, visibility("hidden")))
void SWIFT_CC(swiftasync) swift56override_swift_task_future_wait(
                                            OpaqueValue *,
                                            SWIFT_ASYNC_CONTEXT AsyncContext *,
                                            AsyncTask *,
                                            TaskContinuationFunction *,
                                            AsyncContext *,
                                            TaskFutureWait_t *original);

__attribute__((weak, visibility("hidden")))
void SWIFT_CC(swiftasync) swift56override_swift_task_future_wait_throwing(
                                            OpaqueValue *,
                                            SWIFT_ASYNC_CONTEXT AsyncContext *,
                                            AsyncTask *,
                                            ThrowingTaskFutureWaitContinuationFunction *,
                                            AsyncContext *,
                                            TaskFutureWaitThrowing_t *original);
} // namespace swift
