// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -g %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test

// rdar://106849189 move-only types should be supported in freestanding mode
// UNSUPPORTED: freestanding

// UNSUPPORTED: back_deployment_runtime
// REQUIRES: concurrency_runtime
// REQUIRES: synchronization

import StdlibUnittest
import Synchronization
@_spi(CustomDefaultExecutors) import _Concurrency

typealias DefaultExecutorFactory = SimpleExecutorFactory

struct SimpleExecutorFactory: ExecutorFactory {
  public static var mainExecutor: any MainExecutor {
    print("Creating main executor")
    return SimpleMainExecutor()
  }
  public static var defaultExecutor: any TaskExecutor {
    print("Creating task executor")
    return SimpleTaskExecutor()
  }
}

@available(SwiftStdlib 6.2, *)
final class SimpleMainExecutor: MainExecutor, @unchecked Sendable {
  public var isRunning: Bool = false

  var shouldStop: Bool = false
  let queue = Mutex<[UnownedJob]>([])

  func enqueue(_ job: consuming ExecutorJob) {
    print("Enqueued job")
    let unownedJob = UnownedJob(job)
    queue.withLock {
      $0.append(unownedJob)
    }
  }

  func run() throws {
    print("Running")
    isRunning = true
    while !shouldStop {
      let jobs = queue.withLock {
        let jobs = $0
        $0.removeAll()
        return jobs
      }
      for job in jobs {
        print("Running job")
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
  func enqueue(_ job: consuming ExecutorJob) {
    MainActor.executor.enqueue(job)
  }
}

func myAsyncFunction() async {
  print("Hello World")
}

@available(SwiftStdlib 6.2, *)
@main struct Main {
  static func main() async {
    print("Hello")
    await myAsyncFunction()
    print("Goodbye")
  }
}

// CHECK: Creating main executor
// CHECK-NEXT: Creating task executor
// CHECK-NEXT: Hello
// CHECK-NEXT: Enqueued job
// CHECK-NEXT: Running
// CHECK-NEXT: Running job
// CHECK-NEXT: Hello World
// CHECK-NEXT: Goodbye

