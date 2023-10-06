// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library )

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch
import StdlibUnittest

protocol CountingExecutor: Executor, Sendable {
  var enqueueCount: Int { get }

  func expectEnqueues(_ expected: Int)
}

extension CountingExecutor {
  func expectEnqueues(_ expected: Int) {
    precondition(self.enqueueCount == expected, "Expected [\(expected)] enqueues but had [\(enqueueCount)] (difference: \(enqueueCount - expected))")
  }
}

final class CountEnqueuesSerialExecutor: SerialExecutor, CountingExecutor, @unchecked Sendable {
  var enqueueCount = 0

  func enqueue(_ job: consuming ExecutorJob) {
    enqueueCount += 1
    print("enqueue job")
  }
}

final class CountEnqueuesExecutor: CountingExecutor, @unchecked Sendable {
  var enqueueCount = 0

  func enqueue(_ job: consuming ExecutorJob) {
    enqueueCount += 1
    print("enqueue job")
  }
}


@MainActor
func mainActorMethod() {}

@available(SwiftStdlib 5.9, *)
actor CounterExecutorActor {
  let executor: CountEnqueuesSerialExecutor

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.executor.asUnownedSerialExecutor()
  }

  init(executor: CountEnqueuesSerialExecutor) {
    self.executor = executor
  }

  func test() {
    fatalError("Not expecting to run this")
  }
}

@globalActor
public actor MyGlobalActor: GlobalActor {
  public static let shared = MyGlobalActor()

  let executor = CountEnqueuesSerialExecutor()

  @inlinable
  public nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.executor.asUnownedSerialExecutor()
  }
}

@main struct Main {

  static func main() async {
    if #available(SwiftStdlib 5.9, *) {
      let countingSerialExecutor = CountEnqueuesSerialExecutor()
      let countingExecutor = CountEnqueuesExecutor()

      var expectedSerialEnqueues = 0
      var expectedEnqueues = 0

      do {
        Task(on: countingSerialExecutor) {
          fatalError("never executed")
        }
        expectedSerialEnqueues += 1
        countingSerialExecutor.expectEnqueues(expectedSerialEnqueues)
      }

      do {
        await withTaskGroup(of: Int.self) { group in
          group.addTask(on: countingSerialExecutor) {
            fatalError("never executed")
          }
        }
        expectedSerialEnqueues += 1
        countingSerialExecutor.expectEnqueues(expectedSerialEnqueues)
      }

//      do {
//        Task(on: countingExecutor) {
//          fatalError("never executed")
//        }
//        expectedEnqueues += 1
//        countingExecutor.expectEnqueues(expectedEnqueues)
//      }

      do {
        let actor = CounterExecutorActor(executor: countingSerialExecutor)
        Task(on: actor) { actor in
          actor.test()
          fatalError("never executed")
        }
        expectedSerialEnqueues += 1
        countingSerialExecutor.expectEnqueues(expectedSerialEnqueues)
      }

      do {
        Task(on: MainActor.self) { // specialized method for main actor
          mainActorMethod() // good, no await here
        }
        countingSerialExecutor.expectEnqueues(expectedSerialEnqueues)
        countingExecutor.expectEnqueues(expectedEnqueues)
      }

      do {
        MainActor.task { actor in
          await mainActorMethod() // unfortunate await here, we are on the main actor after all
        }
        MainActor.task { // specialized specifically for the @MainActor
          mainActorMethod()
        }
        countingSerialExecutor.expectEnqueues(expectedSerialEnqueues)
        countingExecutor.expectEnqueues(expectedEnqueues)
      }

      do {
        assert(MyGlobalActor.shared.executor.enqueueCount == 0)
        MyGlobalActor.task { actor in
          // do nothing
        }
        assert(MyGlobalActor.shared.executor.enqueueCount == 1)
      }
    }
  }
}