//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

import Swift

// Store the Timestamp in the executor private data, if it will fit; otherwise,
// use the allocator to allocate space for it and stash a pointer in the private
// data area.
@available(SwiftStdlibCurrentOS 6.2, *)
extension ExecutorJob {
  fileprivate var cooperativeExecutorTimestampIsIndirect: Bool {
    return MemoryLayout<(Int, Int)>.size
      < MemoryLayout<CooperativeExecutor.Timestamp>.size
  }

  fileprivate var cooperativeExecutorTimestampPointer: UnsafeMutablePointer<CooperativeExecutor.Timestamp> {
    get {
      assert(cooperativeExecutorTimestampIsIndirect)
      return unsafe withUnsafeExecutorPrivateData {
        unsafe $0.withMemoryRebound(to: UnsafeMutablePointer<CooperativeExecutor.Timestamp>.self) {
          return unsafe $0[0]
        }
      }
    }
    set {
      assert(cooperativeExecutorTimestampIsIndirect)
      unsafe withUnsafeExecutorPrivateData {
        unsafe $0.withMemoryRebound(to: UnsafeMutablePointer<CooperativeExecutor.Timestamp>.self) {
          unsafe $0[0] = newValue
        }
      }
    }
  }

  fileprivate var cooperativeExecutorTimestamp: CooperativeExecutor.Timestamp {
    get {
      if cooperativeExecutorTimestampIsIndirect {
        let ptr = unsafe cooperativeExecutorTimestampPointer
        return unsafe ptr.pointee
      } else {
        return unsafe withUnsafeExecutorPrivateData {
          return unsafe $0.assumingMemoryBound(
            to: CooperativeExecutor.Timestamp.self
          )[0]
        }
      }
    }
    set {
      if cooperativeExecutorTimestampIsIndirect {
        let ptr = unsafe cooperativeExecutorTimestampPointer
        unsafe ptr.pointee = newValue
     } else {
        unsafe withUnsafeExecutorPrivateData {
          unsafe $0.withMemoryRebound(to: CooperativeExecutor.Timestamp.self) {
            unsafe $0[0] = newValue
          }
        }
      }
    }
  }

  fileprivate mutating func setupCooperativeExecutorTimestamp() {
    // If a Timestamp won't fit, allocate
    if cooperativeExecutorTimestampIsIndirect {
      let ptr: UnsafeMutablePointer<CooperativeExecutor.Timestamp>
      // Try to use the task allocator if it has one
      if let allocator {
        unsafe ptr = allocator.allocate(as: CooperativeExecutor.Timestamp.self)
      } else {
        unsafe ptr = .allocate(capacity: 1)
      }
      unsafe self.cooperativeExecutorTimestampPointer = ptr
    }
  }

  fileprivate mutating func clearCooperativeExecutorTimestamp() {
    // If a Timestamp won't fit, deallocate
    if cooperativeExecutorTimestampIsIndirect {
      let ptr = unsafe self.cooperativeExecutorTimestampPointer
      if let allocator {
        unsafe allocator.deallocate(ptr)
      } else {
        unsafe ptr.deallocate()
      }
    }
  }
}

/// A co-operative executor that can be used as the main executor or as a
/// task executor.
@available(SwiftStdlibCurrentOS 6.2, *)
class CooperativeExecutor: Executor, @unchecked Sendable {
  var runQueue: PriorityQueue<UnownedJob>
  var waitQueue: PriorityQueue<UnownedJob>
  var shouldStop: Bool = false

  /// Internal representation of a duration for CooperativeExecutor
  struct Duration {
    var seconds: Int64
    var nanoseconds: Int64

    init(seconds: Int64, nanoseconds: Int64) {
      self.seconds = seconds
      self.nanoseconds = nanoseconds
    }

    init(from duration: Swift.Duration) {
      let (seconds, attoseconds) = duration.components
      self.seconds = seconds
      self.nanoseconds = attoseconds / 1_000_000_000
    }
  }

  /// Internal representation of a timestamp for CooperativeExecutor
  struct Timestamp: Comparable {
    var seconds: Int64
    var nanoseconds: Int64

    static var zero: Timestamp {
      return Timestamp(seconds: 0, nanoseconds: 0)
    }

    static func == (lhs: Timestamp, rhs: Timestamp) -> Bool {
      return lhs.seconds == rhs.seconds && lhs.nanoseconds == rhs.nanoseconds
    }
    static func < (lhs: Timestamp, rhs: Timestamp) -> Bool {
      return lhs.seconds < rhs.seconds || (
        lhs.seconds == rhs.seconds
          && lhs.nanoseconds < rhs.nanoseconds
      )
    }
    static func - (lhs: Timestamp, rhs: Timestamp) -> Duration {
      if lhs.nanoseconds < rhs.nanoseconds {
        return Duration(seconds: lhs.seconds - rhs.seconds - 1,
                        nanoseconds: 1_000_000_000 + lhs.nanoseconds
                          - rhs.nanoseconds)
      }
      return Duration(seconds: lhs.seconds - rhs.seconds,
                      nanoseconds: lhs.nanoseconds - rhs.nanoseconds)
    }
    static func + (lhs: Timestamp, rhs: Duration) -> Timestamp {
      var seconds = lhs.seconds + rhs.seconds
      var nanoseconds = lhs.nanoseconds + rhs.nanoseconds
      // Normally will run only once
      while nanoseconds > 1_000_000_000 {
        seconds += 1
        nanoseconds -= 1_000_000_000
      }
      return Timestamp(seconds: seconds, nanoseconds: nanoseconds)
    }
  }

  public init() {
    runQueue = PriorityQueue(compare: { $0.priority > $1.priority })
    waitQueue =
      PriorityQueue(compare: {
                      ExecutorJob($0).cooperativeExecutorTimestamp
                        < ExecutorJob($1).cooperativeExecutorTimestamp
                    })
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    runQueue.push(UnownedJob(job))
  }

  public var isMainExecutor: Bool { true }

  public var asSchedulable: any SchedulableExecutor { self }
}

@available(SwiftStdlibCurrentOS 6.2, *)
extension CooperativeExecutor: SchedulableExecutor {
  var currentTime: Timestamp {
    var now: Timestamp = .zero
    unsafe _getTime(seconds: &now.seconds,
                    nanoseconds: &now.nanoseconds,
                    clock: _ClockID.suspending.rawValue)
    return now
  }

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                after delay: C.Duration,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    let duration = Duration(from: clock.convert(from: delay)!)
    let deadline = self.currentTime + duration

    job.setupCooperativeExecutorTimestamp()
    job.cooperativeExecutorTimestamp = deadline
    waitQueue.push(UnownedJob(job))
  }
}

@available(SwiftStdlibCurrentOS 6.2, *)
extension CooperativeExecutor: RunLoopExecutor {
  public func run() throws {
    try runUntil { false }
  }

  public func runUntil(_ condition: () -> Bool) throws {
    shouldStop = false
    while !shouldStop && !condition() {
      // Process the timer queue
      let now = currentTime
      while let job = waitQueue.pop(when: {
                                      ExecutorJob($0).cooperativeExecutorTimestamp <= now
                                    }) {
        var theJob = ExecutorJob(job)
        theJob.clearCooperativeExecutorTimestamp()
        runQueue.push(job)
      }

      // Now run any queued jobs
      while let job = runQueue.pop() {
        unsafe ExecutorJob(job).runSynchronously(
          on: self.asUnownedSerialExecutor()
        )
      }

      // Finally, wait until the next deadline
      if let job = waitQueue.top {
        let deadline = ExecutorJob(job).cooperativeExecutorTimestamp
        let now = self.currentTime
        if deadline > now {
          let toWait = deadline - now
          _sleep(seconds: toWait.seconds,
                 nanoseconds: toWait.nanoseconds)
        }
      } else {
        // Stop if no more jobs are available
        break
      }
    }
  }

  public func stop() {
    shouldStop = true
  }
}

@available(SwiftStdlibCurrentOS 6.2, *)
extension CooperativeExecutor: SerialExecutor {}

@available(SwiftStdlibCurrentOS 6.2, *)
extension CooperativeExecutor: TaskExecutor {}

@available(SwiftStdlibCurrentOS 6.2, *)
extension CooperativeExecutor: MainExecutor {}

#endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
