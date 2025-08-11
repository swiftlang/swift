// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -g %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test

// rdar://106849189 move-only types should be supported in freestanding mode
// UNSUPPORTED: freestanding

// UNSUPPORTED: back_deployment_runtime
// REQUIRES: concurrency_runtime
// REQUIRES: synchronization
// REQUIRES: libdispatch

import StdlibUnittest
import Synchronization
import Dispatch

typealias DefaultExecutorFactory = SimpleExecutorFactory

struct SimpleExecutorFactory: ExecutorFactory {
  public static var mainExecutor: any MainExecutor {
    return SimpleMainExecutor()
  }
  public static var defaultExecutor: any TaskExecutor {
    return SimpleTaskExecutor()
  }
}

@available(SwiftStdlib 6.2, *)
final class SimpleMainExecutor: MainExecutor, @unchecked Sendable {
  public var isRunning: Bool = false

  var shouldStop: Bool = false
  let queue = Mutex<[UnownedJob]>([])

  func enqueue(_ job: consuming ExecutorJob) {
    let unownedJob = UnownedJob(job)
    queue.withLock {
      $0.append(unownedJob)
    }
  }

  func run() throws {
    isRunning = true
    while !shouldStop {
      let jobs = queue.withLock {
        let jobs = $0
        $0.removeAll()
        return jobs
      }
      for job in jobs {
        job.runSynchronously(on: self.asUnownedSerialExecutor())
      }
    }
    isRunning = false
  }

  func stop() {
    shouldStop = true
  }
}

@available(SwiftStdlib 6.2, *)
final class SimpleTaskExecutor: TaskExecutor, @unchecked Sendable {
  let queue = Mutex<[UnownedJob]>([])

  func enqueue(_ job: consuming ExecutorJob) {
    print("Enqueued")
    let unownedJob = UnownedJob(job)
    queue.withLock {
      $0.append(unownedJob)
    }
  }

  func run() throws {
    while true {
      let jobs = queue.withLock {
        let jobs = $0
        $0.removeAll()
        return jobs
      }
      for job in jobs {
        job.runSynchronously(on: self.asUnownedTaskExecutor())
      }
    }
  }
}

@available(SwiftStdlib 6.2, *)
@main struct Main {
  static func main() async {
    DispatchQueue.global().async {
      try! (Task.defaultExecutor as! SimpleTaskExecutor).run()
    }

    await withTaskGroup { group in
      for _ in 0..<3 {
        group.addTask() {
          for _ in 0..<100 {
            await withUnsafeContinuation { cont in
              cont.resume()
            }
          }
        }
      }
    }

  }
}

// If we're on the fast path, we'll only enqueue four times (once per group)

// CHECK: Enqueued
// CHECK: Enqueued
// CHECK: Enqueued
// CHECK: Enqueued
// CHECK-NOT: Enqueued
