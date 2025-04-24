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
import _Concurrency
@_spi(Testing) import _Concurrency
import Synchronization

fileprivate func createJob(priority: TaskPriority = TaskPriority.medium,
               _ body: @escaping () -> ()) -> ExecutorJob {
  let flags: Int = Int(priority.rawValue)
  let (task, _) = Builtin.createAsyncTask(flags, body)
  var job = ExecutorJob(unsafeBitCast(Builtin.convertTaskToJob(task),
                                      to: UnownedJob.self))
  job.priority = JobPriority(priority)
  return job
}

// If we ask to wait for a delay D, this sets the maximum acceptable delay
//
//   T = D + maxDelayTolerance
//
// that we will allow during the test.
//
// Since we run in CI, this value probably needs to be fairly large.  We
// could investigate dropping it for local testing, perhaps.
fileprivate let maxDelayTolerance = 50 // ms

struct ExecutorFixture {
  static func test(executor: some Executor) async -> Bool {
    var result = true
    print("Testing \(executor)")
    if let runLoopExecutor = executor as? any RunLoopExecutor {
      result = await test(runLoopExecutor: runLoopExecutor) && result
    }
    if let serialExecutor = executor as? any SerialExecutor {
      result = await test(serialExecutor: serialExecutor) && result
    }
    #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
    if let schedulableExecutor = executor as? any SchedulableExecutor {
      result = await test(schedulableExecutor: schedulableExecutor) && result
    }
    #endif
    if let taskExecutor = executor as? any TaskExecutor {
      result = await test(taskExecutor: taskExecutor) && result
    }
    if result {
      print("** ALL TESTS PASSED")
    } else {
      print("** TESTS FAILED")
    }
    return result
  }

  static func test(runLoopExecutor: some RunLoopExecutor) async -> Bool {
    print("\no RunLoopExecutor")

    let stopJob = createJob(priority: .medium) {
      print("  - stopping")
      runLoopExecutor.stop()
    }

    runLoopExecutor.enqueue(stopJob)

    print("  - running")
    try! runLoopExecutor.run()

    print("  - stopped")

    return true
  }

  static func test(serialExecutor: some SerialExecutor) async -> Bool {
    print("\no SerialExecutor")

    let order = Mutex<[Int]>([])

    await withCheckedContinuation { continuation in
      let runLoopExecutor = serialExecutor as? RunLoopExecutor

      // Enqueue the jobs from an enqueued job; this means that if the
      // executor does _not_ implement RunLoopExecutor, we can't be caught
      // out and end up running htem in the wrong order.
      //
      // (Otherwise, the executor might be running already, and could start
      // running mediumJob immediately before it has even seen highJob.)
      let enqueueJob = createJob(priority: .medium) {
        let highJob = createJob(priority: .high) {
          print("  - High priority job")
          order.withLock {
            $0.append(1)
          }
        }
        let highJob2 = createJob(priority: .high) {
          print("  - High priority job 2")
          order.withLock {
            $0.append(2)
          }
        }
        let mediumJob = createJob(priority: .medium) {
          print("  - Medium priority job")
          order.withLock {
            $0.append(3)
          }
        }
        let mediumJob2 = createJob(priority: .medium) {
          print("  - Medium priority job 2")
          order.withLock {
            $0.append(4)
          }
        }
        let lowJob = createJob(priority: .low) {
          print("  - Low priority job")
          order.withLock {
            $0.append(5)
          }
        }
        let lowJob2 = createJob(priority: .low) {
          print("  - Low priority job 2")
          order.withLock {
            $0.append(6)
          }
        }
        let stopJob = createJob(priority: .low) {
          print("  - Stopping (low priority job 3)")
          // If this executor implements RunLoopExecutor, stop it
          if let runLoopExecutor {
            print("  - Telling RunLoopExecutor to stop")
            runLoopExecutor.stop()
          }
          continuation.resume()
          order.withLock {
            $0.append(7)
          }
        }

        serialExecutor.enqueue(mediumJob)  // 3
        serialExecutor.enqueue(lowJob)     // 5
        serialExecutor.enqueue(highJob)    // 1
        serialExecutor.enqueue(lowJob2)    // 6
        serialExecutor.enqueue(mediumJob2) // 4
        serialExecutor.enqueue(highJob2)   // 2
        serialExecutor.enqueue(stopJob)    // 7
      }
      serialExecutor.enqueue(enqueueJob)

      // If this executor implements RunLoopExecutor, run it
      if let runLoopExecutor {
        print("  - Telling RunLoopExecutor to run")
        try! runLoopExecutor.run()
      }
    }

    return order.withLock {
      if $0 != [1, 2, 3, 4, 5, 6, 7] {
        print("  * FAILED (\($0))")
        return false
      }
      print("  * OK")
      return true
    }
  }

  #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  static func test<C: Clock, E: SchedulableExecutor> (
      schedulableExecutor: E,
      clock: C
  ) async -> Bool {
    print("\no SchedulableExecutor (\(clock))")

    print("  - Delays")
    let delays = [ 15, 30, 100, 250, 500, 1000 ]
    let delayOrder = Mutex<[Int]>([])
    let actualDelays = Mutex<[C.Duration]>([])
    let start = clock.now

    await withCheckedContinuation { continuation in
      let runLoopExecutor = schedulableExecutor as? RunLoopExecutor

      for delay in delays.reversed() {
        let job = createJob(priority: .medium) {
          let actualDelay = start.duration(to: clock.now)
          print("    + Delayed job (\(delay) ms, actual \(actualDelay))")
          delayOrder.withLock {
            $0.append(delay)
          }
          actualDelays.withLock {
            $0.append(actualDelay)
          }
        }
        schedulableExecutor.enqueue(job,
                                    after: .milliseconds(delay),
                                    clock: .suspending)
      }
      let stopJob = createJob(priority: .low) {
        print("    + Stopping")
        if let runLoopExecutor {
          print("    + Telling RunLoopExecutor to stop")
          runLoopExecutor.stop()
        }
        continuation.resume()
      }
      schedulableExecutor.enqueue(stopJob,
                                  after: .milliseconds(1100),
                                  clock: .suspending)
      if let runLoopExecutor {
        print("    + Telling RunLoopExecutor to run")
        try! runLoopExecutor.run()
      }
    }

    let result = delayOrder.withLock {
      if $0 != delays {
        print("  * FAILED (\($0))")
        return false
      }
      for (ndx, delay) in delays.enumerated() {
        let actualDelay = clock.convert(from: actualDelays.withLock {
                                          return $0[ndx]
                                        })!

        // Must not return before the requested delay
        let minDelay: Duration = .milliseconds(delay)

        // Must not return more than 25% after the requested delay
        let maxDelay: Duration = .milliseconds(delay + max(delay / 4,
                                                           maxDelayTolerance))

        if actualDelay < minDelay {
          print("  * FAILED (\(actualDelay) < \(minDelay))")
          return false
        }

        if actualDelay > maxDelay {
          print("  * FAILED (\(actualDelay) > \(maxDelay))")
          return false
        }
      }
      print("  * OK")
      return true
    }

    print("  - Timestamps")
    let baseTime = clock.now
    let targets = delays.map {
      baseTime.advanced(by: clock.convert(from: .milliseconds($0))!)
    }
    let actualTimes = Mutex<[C.Instant]>([])
    let timeOrder = Mutex<[C.Instant]>([])

    await withCheckedContinuation { continuation in
      let runLoopExecutor = schedulableExecutor as? RunLoopExecutor

      for target in targets.reversed() {
        let job = createJob(priority: .medium) {
          let actualTime = clock.now
          print("    + Targeted job (target \(target), actual \(actualTime))")
          timeOrder.withLock {
            $0.append(target)
          }
          actualTimes.withLock {
            $0.append(actualTime)
          }
        }
        schedulableExecutor.enqueue(job,
                                    at: target,
                                    clock: clock)
      }
      let stopJob = createJob(priority: .low) {
        print("    + Stopping")
        if let runLoopExecutor {
          print("    + Telling RunLoopExecutor to stop")
          runLoopExecutor.stop()
        }
        continuation.resume()
      }
      schedulableExecutor.enqueue(stopJob,
                                  after: .milliseconds(1100),
                                  clock: .suspending)
      if let runLoopExecutor {
        print("    + Telling RunLoopExecutor to run")
        try! runLoopExecutor.run()
      }
    }

    let result2 = timeOrder.withLock {
      if $0 != targets {
        print("  * FAILED (\($0))")
        return false
      }
      for (ndx, target) in targets.enumerated() {
        let actualTime = actualTimes.withLock { return $0[ndx] }

        // Must not return before the requested time
        let minTime = target

        // Must not return more than 25% after the requested time
        let maxTime = target.advanced(
          by: clock.convert(from: .milliseconds(max(delays[ndx] / 4,
                                                    maxDelayTolerance)))!
        )

        if actualTime < minTime {
          print("  * FAILED (\(actualTime) < \(minTime))")
          return false
        }

        if actualTime > maxTime {
          print("  * FAILED (\(actualTime) > \(maxTime))")
          return false
        }
      }
      print("  * OK")
      return true
    }

    return result && result2
  }
  static func test(schedulableExecutor: some SchedulableExecutor) async -> Bool {
    let result = await test(schedulableExecutor: schedulableExecutor,
                            clock: .suspending)
    let result2 = await test(schedulableExecutor: schedulableExecutor,
                             clock: .continuous)
    return result && result2
  }
  #endif

  static func test(taskExecutor: some TaskExecutor) async -> Bool {
    print("\no TaskExecutor")

    let counter = Mutex<Int>(30)

    await withCheckedContinuation { continuation in
      let runLoopExecutor = taskExecutor as? RunLoopExecutor

      for _ in 0..<10 {
        let highJob = createJob(priority: .high) {
          print("  - High priority job")
          counter.withLock {
            $0 = $0 - 1
            if $0 == 0 {
              if let runLoopExecutor {
                print("  - Telling RunLoopExecutor to stop")
                runLoopExecutor.stop()
              }
              continuation.resume()
            }
          }
        }
        let mediumJob = createJob(priority: .medium) {
          print("  - Medium priority job")
          counter.withLock {
            $0 = $0 - 1
            if $0 == 0 {
              if let runLoopExecutor {
                print("  - Telling RunLoopExecutor to stop")
                runLoopExecutor.stop()
              }
              continuation.resume()
            }
          }
        }
        let lowJob = createJob(priority: .low) {
          print("  - Low priority job")
          counter.withLock {
            $0 = $0 - 1
            if $0 == 0 {
              if let runLoopExecutor {
                print("  - Telling RunLoopExecutor to stop")
                runLoopExecutor.stop()
              }
              continuation.resume()
            }
          }
        }

        taskExecutor.enqueue(highJob)
        taskExecutor.enqueue(mediumJob)
        taskExecutor.enqueue(lowJob)
      }

      if let runLoopExecutor {
        print("  - Telling RunLoopExecutor to run")
        try! runLoopExecutor.run()
      }
    }

    print("  * OK")

    return true
  }
}
