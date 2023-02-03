// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime

import Dispatch

func checkIfMainQueue(expectedAnswer expected: Bool) {
  dispatchPrecondition(condition: expected ? .onQueue(DispatchQueue.main)
      : .notOnQueue(DispatchQueue.main))
}

protocol MyExecutor: SerialExecutor {}

protocol UseMyExecutor: Actor {
  nonisolated var executor: MyExecutor { get }
}

extension UseMyExecutor {
  /// Implement the Actor protocol requirement here
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.executor.asUnownedSerialExecutor()
  }
}

actor ExecutorFromProtocolActor: UseMyExecutor {
  nonisolated let executor: MyExecutor

  init(executor: MyExecutor) {
    self.executor = executor
  }

  func test() {
    checkIfMainQueue(expectedAnswer: true)
    print("\(Self.self) on main queue")
  }
}

@main struct MyProgram {
  static func main() async throws {
    checkIfMainQueue(expectedAnswer: true)
    await ExecutorFromProtocolActor(MainActor.sharedUnownedExecutor).test()
    checkIfMainQueue(expectedAnswer: true)
    // CHECK: ActorOnPassedExecutor on main queue
  }
}