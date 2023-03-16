//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

/// A service that can execute jobs.
@available(SwiftStdlib 5.1, *)
public protocol Executor: AnyObject, Sendable {

  @available(macOS, introduced: 10.15, deprecated: 9999, message: "Implement 'enqueue(_: __owned Job)' instead")
  @available(iOS, introduced: 13.0, deprecated: 9999, message: "Implement 'enqueue(_: __owned Job)' instead")
  @available(watchOS, introduced: 6.0, deprecated: 9999, message: "Implement 'enqueue(_: __owned Job)' instead")
  @available(tvOS, introduced: 13.0, deprecated: 9999, message: "Implement 'enqueue(_: __owned Job)' instead")
  func enqueue(_ job: UnownedJob)

  @available(SwiftStdlib 5.9, *)
  func enqueue(_ job: __owned Job)
}

/// A service that executes jobs.
@available(SwiftStdlib 5.1, *)
public protocol SerialExecutor: Executor {
  // This requirement is repeated here as a non-override so that we
  // get a redundant witness-table entry for it.  This allows us to
  // avoid drilling down to the base conformance just for the basic
  // work-scheduling operation.
  @_nonoverride
  @available(macOS, introduced: 10.15, deprecated: 9999, message: "Implement 'enqueue(_: __owned Job)' instead")
  @available(iOS, introduced: 13.0, deprecated: 9999, message: "Implement 'enqueue(_: __owned Job)' instead")
  @available(watchOS, introduced: 6.0, deprecated: 9999, message: "Implement 'enqueue(_: __owned Job)' instead")
  @available(tvOS, introduced: 13.0, deprecated: 9999, message: "Implement 'enqueue(_: __owned Job)' instead")
  func enqueue(_ job: UnownedJob)

  // This requirement is repeated here as a non-override so that we
  // get a redundant witness-table entry for it.  This allows us to
  // avoid drilling down to the base conformance just for the basic
  // work-scheduling operation.
  @_nonoverride
  @available(SwiftStdlib 5.9, *)
  func enqueue(_ job: __owned Job)

  /// Convert this executor value to the optimized form of borrowed
  /// executor references.
  @available(SwiftStdlib 5.9, *)
  func asUnownedSerialExecutor() -> UnownedSerialExecutor
}

@available(SwiftStdlib 5.9, *)
extension Executor {
  public func enqueue(_ job: UnownedJob) {
    self.enqueue(Job(job))
  }

  public func enqueue(_ job: __owned Job) {
    self.enqueue(UnownedJob(job))
  }
}

@available(SwiftStdlib 5.9, *)
extension SerialExecutor {
  @available(SwiftStdlib 5.9, *)
  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

/// An unowned reference to a serial executor (a `SerialExecutor`
/// value).
///
/// This is an optimized type used internally by the core scheduling
/// operations.  It is an unowned reference to avoid unnecessary
/// reference-counting work even when working with actors abstractly.
/// Generally there are extra constraints imposed on core operations
/// in order to allow this.  For example, keeping an actor alive must
/// also keep the actor's associated executor alive; if they are
/// different objects, the executor must be referenced strongly by the
/// actor.
@available(SwiftStdlib 5.1, *)
@frozen
public struct UnownedSerialExecutor: Sendable {
  #if compiler(>=5.5) && $BuiltinExecutor
  @usableFromInline
  internal var executor: Builtin.Executor

  @_spi(ConcurrencyExecutors)
  @available(SwiftStdlib 5.9, *)
  public var _executor: Builtin.Executor {
    self.executor
  }
  #endif

  @inlinable
  internal init(_ executor: Builtin.Executor) {
    #if compiler(>=5.5) && $BuiltinExecutor
    self.executor = executor
    #endif
  }

  @inlinable
  public init<E: SerialExecutor>(ordinary executor: __shared E) {
    #if compiler(>=5.5) && $BuiltinBuildExecutor
    self.executor = Builtin.buildOrdinarySerialExecutorRef(executor)
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
  }

}

/// Primarily a debug utility.
///
/// If the passed in Job is a Task, returns the complete 64bit TaskId,
/// otherwise returns only the job's 32bit Id.
///
/// - Returns: the Id stored in this Job or Task, for purposes of debug printing
@available(SwiftStdlib 5.9, *)
@_silgen_name("swift_task_getJobTaskId")
internal func _getJobTaskId(_ job: UnownedJob) -> UInt64

// Used by the concurrency runtime
@available(SwiftStdlib 5.1, *)
@_silgen_name("_swift_task_enqueueOnExecutor")
internal func _enqueueOnExecutor<E>(job unownedJob: UnownedJob, executor: E)
where E: SerialExecutor {
  if #available(SwiftStdlib 5.9, *) {
    executor.enqueue(Job(context: unownedJob._context))
  } else {
    executor.enqueue(unownedJob)
  }
}

#if !SWIFT_STDLIB_SINGLE_THREADED_CONCURRENCY && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
// This must take a DispatchQueueShim, not something like AnyObject,
// or else SILGen will emit a retain/release in unoptimized builds,
// which won't work because DispatchQueues aren't actually
// Swift-retainable.
@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_enqueueOnDispatchQueue")
internal func _enqueueOnDispatchQueue(_ job: UnownedJob,
                                      queue: DispatchQueueShim)

/// Used by the runtime solely for the witness table it produces.
/// FIXME: figure out some way to achieve that which doesn't generate
/// all the other metadata
///
/// Expected to work for any primitive dispatch queue; note that this
/// means a dispatch_queue_t, which is not the same as DispatchQueue
/// on platforms where that is an instance of a wrapper class.
@available(SwiftStdlib 5.1, *)
internal final class DispatchQueueShim: @unchecked Sendable, SerialExecutor {
  func enqueue(_ job: UnownedJob) {
    _enqueueOnDispatchQueue(job, queue: self)
  }

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    return UnownedSerialExecutor(ordinary: self)
  }
}
#endif

// ==== -----------------------------------------------------------------------
// - MARK: Executor assertions

/// Checks if the current task is running on the expected executor.
///
/// Do note that if multiple actors share the same serial executor,
/// this assertion checks for the executor, not specific actor instance.
///
/// Generally, Swift programs should be constructed such that it is statically
/// known that a specific executor is used, for example by using global actors or
/// custom executors. However, in some APIs it may be useful to provide an
/// additional runtime check for this, especially when moving towards Swift
/// concurrency from other runtimes which frequently use such assertions.
@available(SwiftStdlib 5.9, *)
public func preconditionOnSerialExecutor(
    _ executor: some SerialExecutor,
    _ message: @autoclosure () -> String = "",
    file: StaticString = #fileID, line: UInt = #line) {
  preconditionOnSerialExecutor(executor.asUnownedSerialExecutor(), file: file, line: line)
}

@available(SwiftStdlib 5.9, *)
public func preconditionOnSerialExecutor(
    _ unowned: UnownedSerialExecutor,
    _ message: @autoclosure () -> String = "",
    file: StaticString = #fileID, line: UInt = #line) {
  if _taskIsCurrentExecutor(unowned.executor) {
    return
  }

  // TODO: log on what executor it was instead of the expected one
  let message = "Expected executor \(unowned); \(message())"
  preconditionFailure(
      message,
      file: file, line: line)
}

/// Same as ``preconditionOnSerialExecutor(_:_:file:line)`` however only in DEBUG mode.
@available(SwiftStdlib 5.9, *)
public func assertOnSerialExecutor(
    _ executor: some SerialExecutor,
    _ message: @autoclosure () -> String = "",
    file: StaticString = #fileID, line: UInt = #line) {
  assertOnSerialExecutor(executor.asUnownedSerialExecutor(), file: file, line: line)
}

@available(SwiftStdlib 5.9, *)
public func assertOnSerialExecutor(
    _ unowned: UnownedSerialExecutor,
    _ message: @autoclosure () -> String = "",
    file: StaticString = #fileID, line: UInt = #line) {
  if _isDebugAssertConfiguration() {
    if _taskIsCurrentExecutor(unowned.executor) {
      return
    }

    // TODO: log on what executor it was instead of the expected one
    // TODO: fixme use assertion here?
    fatalError("Expected executor \(unowned); \(message())", file: file, line: line)
  }
}

/// Checks if the current task is running on the expected executor.
///
/// Generally, Swift programs should be constructed such that it is statically
/// known that a specific executor is used, for example by using global actors or
/// custom executors. However, in some APIs it may be useful to provide an
/// additional runtime check for this, especially when moving towards Swift
/// concurrency from other runtimes which frequently use such assertions.
/// - Parameter executor: The expected executor.
@available(SwiftStdlib 5.9, *)
@_silgen_name("swift_task_isOnExecutor")
func _taskIsOnExecutor(_ executor: some SerialExecutor) -> Bool

@available(SwiftStdlib 5.1, *)
@_transparent
public // COMPILER_INTRINSIC
func _checkExpectedExecutor(_filenameStart: Builtin.RawPointer,
                            _filenameLength: Builtin.Word,
                            _filenameIsASCII: Builtin.Int1,
                            _line: Builtin.Word,
                            _executor: Builtin.Executor) {
  if _taskIsCurrentExecutor(_executor) {
    return
  }

  _reportUnexpectedExecutor(
    _filenameStart, _filenameLength, _filenameIsASCII, _line, _executor)
}

@available(SwiftStdlib 5.9, *)
@_alwaysEmitIntoClient // FIXME: use @backDeploy(before: SwiftStdlib 5.9)
func _checkExpectedExecutor(
    _ _executor: Builtin.Executor,
    file: String,
    line: Int) {
  if _taskIsCurrentExecutor(_executor) {
    return
  }

  file.utf8CString.withUnsafeBufferPointer { (_ bufPtr: UnsafeBufferPointer<CChar>) in
    let fileBasePtr: Builtin.RawPointer = bufPtr.baseAddress!._rawValue

    // string lengths exclude trailing \0 byte, which should be there!
    let fileLength: Builtin.Word = (bufPtr.count - 1)._builtinWordValue

    // we're handing it UTF-8
    let falseByte: Int8 = 0
    let fileIsASCII: Builtin.Int1 = Builtin.trunc_Int8_Int1(falseByte._value)

    _reportUnexpectedExecutor(
        fileBasePtr, fileLength, fileIsASCII,
        line._builtinWordValue,
        _executor)
  }
}
