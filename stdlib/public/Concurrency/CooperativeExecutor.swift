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

import Swift

extension ExecutorJob {
  fileprivate var cooperativeExecutorTimestamp: CooperativeExecutor.Timestamp {
    get {
      return unsafe withUnsafeExecutorPrivateData {
        return unsafe $0.assumingMemoryBound(
          to: CooperativeExecutor.Timestamp.self
        )[0]
      }
    }
    set {
      unsafe withUnsafeExecutorPrivateData {
        unsafe $0.withMemoryRebound(to: CooperativeExecutor.Timestamp.self) {
          unsafe $0[0] = newValue
        }
      }
    }
  }
}

/// A co-operative executor that can be used as the main executor or as a
/// task executor.
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

    job.cooperativeExecutorTimestamp = deadline
    waitQueue.push(UnownedJob(job))
  }
}

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

extension CooperativeExecutor: SerialExecutor {}

extension CooperativeExecutor: TaskExecutor {}

extension CooperativeExecutor: MainExecutor {}
