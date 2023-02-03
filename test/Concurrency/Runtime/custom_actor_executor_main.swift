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

actor ActorOnSharedMainExecutor {
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    MainActor.sharedUnownedExecutor
  }

  func test() {
    checkIfMainQueue(expectedAnswer: true)
    print("\(Self.self) on main queue")
  }
}

final class UnownedSerialExecutorBox: @unchecked Sendable {
  let value: UnownedSerialExecutor
  init(_ value: UnownedSerialExecutor) {
    self.value = value
  }
}

// Weird idea... technically such pattern would allow "run on this specific event loop"
actor ActorOnPassedExecutor {
  let unownedExecutorBox: UnownedSerialExecutorBox
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.unownedExecutorBox.value
  }

  init(_ executor: UnownedSerialExecutor) {
    self.unownedExecutorBox = .init(executor)
  }

  func test() {
    checkIfMainQueue(expectedAnswer: true)
    print("\(Self.self) on main queue")
  }
}

@main struct MyProgram {

  static func main() async throws {
    checkIfMainQueue(expectedAnswer: true)
    await ActorOnSharedMainExecutor().test()
    checkIfMainQueue(expectedAnswer: true)
    // CHECK: ActorOnSharedMainExecutor on main queue

    checkIfMainQueue(expectedAnswer: true)
    await ActorOnPassedExecutor(MainActor.sharedUnownedExecutor).test()
    checkIfMainQueue(expectedAnswer: true)
    // CHECK: ActorOnPassedExecutor on main queue
  }
}