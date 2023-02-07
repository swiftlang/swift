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

final class InlineExecutor: SpecifiedExecutor, Swift.CustomStringConvertible {
  let name: String

  init(_ name: String) {
    self.name = name
  }

  public func enqueue(_ job: UnownedJob) {
    print("\(self): enqueue")
    job._runSynchronously(on: self.asUnownedSerialExecutor())
    print("\(self): after run")
  }

  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    return UnownedSerialExecutor(ordinary: self)
  }

  var description: Swift.String {
    "InlineExecutor(\(name))"
  }
}

actor MyActor: WithSpecifiedExecutor {

  nonisolated let executor: SpecifiedExecutor

  // Note that we don't have to provide the unownedExecutor in the actor itself.
  // We obtain it from the extension on `WithSpecifiedExecutor`.

  init(executor: SpecifiedExecutor) {
    self.executor = executor
  }

  func test(expectedExecutor: some SerialExecutor) {
    checkIfMainQueue(expectedAnswer: true)
    print("\(Self.self): on executor \(expectedExecutor)")
  }
}

@main struct Main {
  static func main() async {
    print("begin")
    let one = InlineExecutor("one")
    let actor = MyActor(executor: one)
    await actor.test(expectedExecutor: one)
    await actor.test(expectedExecutor: one)
    await actor.test(expectedExecutor: one)
    print("end")
  }
}

// CHECK:      begin
// CHECK-NEXT: InlineExecutor(one): enqueue
// CHECK-NEXT: MyActor: on executor InlineExecutor(one)
// CHECK-NEXT: MyActor: on executor InlineExecutor(one)
// CHECK-NEXT: MyActor: on executor InlineExecutor(one)
// CHECK-NEXT: InlineExecutor(one): after run
// CHECK-NEXT: end
