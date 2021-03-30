//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift
@_implementationOnly import _SwiftConcurrencyShims

// ==== Async Let -------------------------------------------------------------
// Only has internal / builtin functions as it is not really accessible directly

//@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
//@_silgen_name("swift_asyncLet_create")
//func _asyncLetCreate(task: Builtin.NativeObject) -> Builtin.RawPointer

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public func _asyncLetStart<T>(
  operation: __owned @Sendable @escaping () async throws -> T
) async -> Builtin.RawPointer {
  let currentTask = Builtin.getCurrentAsyncTask()

  // Set up the job flags for a new task.
  var flags = Task.JobFlags()
  flags.kind = .task
  flags.priority = getJobFlags(currentTask).priority
  flags.isFuture = true
  flags.isChildTask = true

  print("\(#function) - FLAGS = \(flags.bits)")
//  // Create the asynchronous task future.
//  let (task, _) = Builtin.createAsyncTaskFuture(flags.bits, operation)
//
//  // Initialization of an async let already takes care of registering it with the parent task.
//  let asyncLet = Builtin.createAsyncLet(operation)
//  let asyncLet = Builtin.createAsyncLet(flags.bits, operation)
//  let (asyncLet, task) = Builtin.createAsyncLet(flags.bits, operation)
  let (asyncLet, _) = Builtin.createAsyncLet(flags.bits, operation)
//  assert(unsafeBitCast(asyncLet, to: Int64.self) == unsafeBitCast(two, to: Int64.self))

  // Enqueue the resulting job.
//  _enqueueJobGlobal(
//      Builtin.convertTaskToJob(
//          _asyncLetExtractTask(of: asyncLet)))

//  _enqueueJobGlobal(Builtin.convertTaskToJob(task))

  return asyncLet
}

/// Similar to _taskFutureGet but for AsyncLet
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_asyncLet_wait")
public func _asyncLetGet<T>(asyncLet: Builtin.RawPointer) async -> T

///// Similar to _taskFutureGetThrowing but for AsyncLet
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_asyncLet_wait_throwing")
public func _asyncLetGetThrowing<T>(asyncLet: Builtin.RawPointer) async throws -> T

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@_silgen_name("swift_asyncLet_end")
public func _asyncLetEnd(
  asyncLet: Builtin.RawPointer // TODO: should this take __owned?
)


@_silgen_name("swift_asyncLet_extractTask")
func _asyncLetExtractTask(
  of asyncLet: Builtin.RawPointer
) -> Builtin.NativeObject
