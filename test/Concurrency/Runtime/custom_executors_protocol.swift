// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: libdispatch
// UNSUPPORTED: freestanding

// UNSUPPORTED: back_deployment_runtime
// REQUIRES: concurrency_runtime

import Dispatch

func checkIfMainQueue(expectedAnswer expected: Bool) {
  dispatchPrecondition(condition: expected ? .onQueue(DispatchQueue.main)
      : .notOnQueue(DispatchQueue.main))
}

protocol WithSpecifiedExecutor: Actor {
  nonisolated var executor: SpecifiedExecutor { get }
}

protocol SpecifiedExecutor: SerialExecutor {}

extension WithSpecifiedExecutor {
  /// Establishes the WithSpecifiedExecutorExecutor as the serial
  /// executor that will coordinate execution for the actor.
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    executor.asUnownedSerialExecutor()
  }
}

final class InlineExecutor: SpecifiedExecutor {
  let name: String

  init(_ name: String) {
    self.name = name
  }

  public func enqueue(_ job: UnownedJob) {
    print("enqueue")
    job._runSynchronously(on: self.asUnownedSerialExecutor())
  }

  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    return UnownedSerialExecutor(ordinary: self)
  }
}

actor OtherActor {
  func test() {
//    checkIfMainQueue(expectedAnswer: false)
    print("\(Self.self) on executor")
  }
}

actor MyActor: WithSpecifiedExecutor {
  let other: OtherActor

  nonisolated let executor: SpecifiedExecutor

  /// FIXME: Compiler should not require us to restate this here. We'll fix it.
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    executor.asUnownedSerialExecutor()
  }

  init(executor: SpecifiedExecutor) {
    self.executor = executor
    self.other = OtherActor()
  }

  func test() async {
    checkIfMainQueue(expectedAnswer: true)
    print("\(Self.self) on executor")
    await other.test()
  }
}

@main struct Main {
  static func main() async {
    print("begin")
    let one = InlineExecutor("one")
    let actor = MyActor(executor: one)
    await actor.test()
    await actor.test()
    await actor.test()
    print("end")
  }
}

// CHECK:      begin
// CHECK-NEXT: InlineExecutor: enqueue
// CHECK-NEXT: MyActor: on executor: inline-executor-one
// CHECK-NEXT: MyActor: on executor: inline-executor-one
// CHECK-NEXT: MyActor: on executor: inline-executor-one
// CHECK-NEXT: end
