//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift
@_implementationOnly import _SwiftConcurrencyShims

// TODO(swift): rename the file to Job.swift eventually, we don't use PartialTask terminology anymore

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_job_run")
@usableFromInline
internal func _swiftJobRun(_ job: UnownedJob,
                           _ executor: UnownedSerialExecutor) -> ()

// ==== -----------------------------------------------------------------------
// MARK: UnownedJob

/// A unit of scheduleable work.
///
/// Unless you're implementing a scheduler,
/// you don't generally interact with jobs directly.
///
/// An `UnownedJob` must be eventually run *exactly once* using ``runSynchronously(on:)``.
/// Not doing so is effectively going to leak and "hang" the work that the job represents (e.g. a ``Task``).
@available(SwiftStdlib 5.1, *)
@frozen
public struct UnownedJob: Sendable {
  private var context: Builtin.Job

  @usableFromInline
  @available(SwiftStdlib 5.9, *)
  internal init(context: Builtin.Job) {
    self.context = context
  }

  /// Create an `UnownedJob` whose lifetime must be managed carefully until it is run exactly once.
  @available(SwiftStdlib 5.9, *)
  public init(_ job: __owned Job) {
    self.context = job.context
  }

  /// The priority of this job.
  @available(SwiftStdlib 5.9, *)
  public var priority: JobPriority {
    let raw = _swift_concurrency_jobPriority(self)
    return JobPriority(rawValue: raw)
  }

  @available(SwiftStdlib 5.9, *)
  internal var _context: Builtin.Job {
    context
  }

  /// Deprecated API to run a job on a specific executor.
  @_alwaysEmitIntoClient
  @inlinable
  @available(*, deprecated, renamed: "Job.runSynchronously(on:)")
  public func _runSynchronously(on executor: UnownedSerialExecutor) {
    _swiftJobRun(self, executor)
  }

  /// Run this job on the passed in executor.
  ///
  /// This operation runs the job on the calling thread and *blocks* until the job completes.
  /// The intended use of this method is for an executor to determine when and where it
  /// wants to run the job and then call this method on it.
  ///
  /// The passed in executor reference is used to establish the executor context for the job,
  /// and should be the same executor as the one semantically calling the `runSynchronously` method.
  ///
  /// - Parameter executor: the executor this job will be semantically running on.
  @_alwaysEmitIntoClient
  @inlinable
  public func runSynchronously(on executor: UnownedSerialExecutor) {
    _swiftJobRun(self, executor)
  }

}

@available(SwiftStdlib 5.9, *)
extension UnownedJob: CustomStringConvertible {
  @available(SwiftStdlib 5.9, *)
  public var description: String {
    let id = _getJobTaskId(self)
    /// Tasks are always assigned an unique ID, however some jobs may not have it set,
    /// and it appearing as 0 for _different_ jobs may lead to misunderstanding it as
    /// being "the same 0 id job", we specifically print 0 (id not set) as nil.
    if (id > 0) {
      return "\(Self.self)(id: \(id))"
    } else {
      return "\(Self.self)(id: nil)"
    }
  }
}

/// A unit of scheduleable work.
///
/// Unless you're implementing a scheduler,
/// you don't generally interact with jobs directly.
@available(SwiftStdlib 5.9, *)
@frozen
@_moveOnly
public struct Job: Sendable {
  internal var context: Builtin.Job

  @usableFromInline
  internal init(context: __owned Builtin.Job) {
    self.context = context
  }

  public init(_ job: UnownedJob) {
    self.context = job._context
  }

  public var priority: JobPriority {
    let raw = _swift_concurrency_jobPriority(UnownedJob(context: self.context))
    return JobPriority(rawValue: raw)
  }

  // TODO: move only types cannot conform to protocols, so we can't conform to CustomStringConvertible;
  //       we can still offer a description to be called explicitly though.
  public var description: String {
    let id = _getJobTaskId(UnownedJob(context: self.context))
    /// Tasks are always assigned an unique ID, however some jobs may not have it set,
    /// and it appearing as 0 for _different_ jobs may lead to misunderstanding it as
    /// being "the same 0 id job", we specifically print 0 (id not set) as nil.
    if (id > 0) {
      return "\(Self.self)(id: \(id))"
    } else {
      return "\(Self.self)(id: nil)"
    }
  }
}

@available(SwiftStdlib 5.9, *)
extension Job {

  /// Run this job on the passed in executor.
  ///
  /// This operation runs the job on the calling thread and *blocks* until the job completes.
  /// The intended use of this method is for an executor to determine when and where it
  /// wants to run the job and then call this method on it.
  ///
  /// The passed in executor reference is used to establish the executor context for the job,
  /// and should be the same executor as the one semantically calling the `runSynchronously` method.
  ///
  /// This operation consumes the job, preventing it accidental use after it has ben run.
  ///
  /// Converting a `Job` to an ``UnownedJob`` and invoking ``UnownedJob/runSynchronously(_:)` on it multiple times is undefined behavior,
  /// as a job can only ever be run once, and must not be accessed after it has been run.
  ///
  /// - Parameter executor: the executor this job will be semantically running on.
  @_alwaysEmitIntoClient
  @inlinable
  __consuming public func runSynchronously(on executor: UnownedSerialExecutor) {
    _swiftJobRun(UnownedJob(self), executor)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: JobPriority

/// The priority of this job.
///
/// The executor determines how priority information affects the way tasks are scheduled.
/// The behavior varies depending on the executor currently being used.
/// Typically, executors attempt to run tasks with a higher priority
/// before tasks with a lower priority.
/// However, the semantics of how priority is treated are left up to each
/// platform and `Executor` implementation.
///
/// A Job's priority is roughly equivalent to a `TaskPriority`,
/// however, since not all jobs are tasks, represented as separate type.
///
/// Conversions between the two priorities are available as initializers on the respective types.
@available(SwiftStdlib 5.9, *)
public struct JobPriority: Sendable {
  public typealias RawValue = UInt8

  /// The raw priority value.
  public var rawValue: RawValue
}

@available(SwiftStdlib 5.9, *)
extension TaskPriority {
  /// Convert this ``UnownedJob/Priority`` to a ``TaskPriority``.
  ///
  /// Most values are directly interchangeable, but this initializer reserves the right to fail for certain values.
  @available(SwiftStdlib 5.9, *)
  public init?(_ p: JobPriority) {
    guard p.rawValue != 0 else {
      /// 0 is "undefined"
      return nil
    }
    self = TaskPriority(rawValue: p.rawValue)
  }
}

@available(SwiftStdlib 5.9, *)
extension JobPriority: Equatable {
  public static func == (lhs: JobPriority, rhs: JobPriority) -> Bool {
    lhs.rawValue == rhs.rawValue
  }

  public static func != (lhs: JobPriority, rhs: JobPriority) -> Bool {
    lhs.rawValue != rhs.rawValue
  }
}

@available(SwiftStdlib 5.9, *)
extension JobPriority: Comparable {
  public static func < (lhs: JobPriority, rhs: JobPriority) -> Bool {
    lhs.rawValue < rhs.rawValue
  }

  public static func <= (lhs: JobPriority, rhs: JobPriority) -> Bool {
    lhs.rawValue <= rhs.rawValue
  }

  public static func > (lhs: JobPriority, rhs: JobPriority) -> Bool {
    lhs.rawValue > rhs.rawValue
  }

  public static func >= (lhs: JobPriority, rhs: JobPriority) -> Bool {
    lhs.rawValue >= rhs.rawValue
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Runtime functions

@available(SwiftStdlib 5.9, *)
@_silgen_name("swift_concurrency_jobPriority")
internal func _swift_concurrency_jobPriority(_ job: UnownedJob) -> UInt8
