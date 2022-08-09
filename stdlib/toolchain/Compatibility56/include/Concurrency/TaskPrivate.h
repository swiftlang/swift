//===--- TaskPrivate.h - Concurrency library internal interface -*- C++ -*-===//
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
// Internal functions for the concurrency library.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CONCURRENCY_TASKPRIVATE_BACKDEPLOY56_H
#define SWIFT_CONCURRENCY_TASKPRIVATE_BACKDEPLOY56_H

#include "Concurrency/Error.h"
#include "Concurrency/Task.h"
#include "swift/Runtime/Error.h"

namespace swift {
namespace {

/// The layout of a context to call one of the following functions:
///
///   @_silgen_name("swift_task_future_wait")
///   func _taskFutureGet<T>(_ task: Builtin.NativeObject) async -> T
///
///   @_silgen_name("swift_task_future_wait_throwing")
///   func _taskFutureGetThrowing<T>(_ task: Builtin.NativeObject) async throws -> T
///
///   @_silgen_name("swift_asyncLet_wait")
///   func _asyncLetGet<T>(_ task: Builtin.RawPointer) async -> T
///
///   @_silgen_name("swift_asyncLet_waitThrowing")
///   func _asyncLetGetThrowing<T>(_ task: Builtin.RawPointer) async throws -> T
///
///   @_silgen_name("swift_taskGroup_wait_next_throwing")
///   func _taskGroupWaitNext<T>(group: Builtin.RawPointer) async throws -> T?
///
class TaskFutureWaitAsyncContext : public AsyncContext {
public:
  SwiftError *errorResult;

  OpaqueValue *successResultPointer;

  void fillWithSuccess(AsyncTask::FutureFragment *future) {
    fillWithSuccess(future->getStoragePtr(), future->getResultType(),
                    successResultPointer);
  }
  void fillWithSuccess(OpaqueValue *src, const Metadata *successType,
                       OpaqueValue *result) {
    successType->vw_initializeWithCopy(result, src);
  }

  void fillWithError(AsyncTask::FutureFragment *future) {
    fillWithError(future->getError());
  }
  void fillWithError(SwiftError *error) {
    errorResult = error;
    swift_errorRetain(error);
  }
};

}
} // namespace swift

#endif // SWIFT_CONCURRENCY_TASKPRIVATE_BACKDEPLOY56_H
