// RUN: %target-run-simple-swift( -Xfrontend -enable-experimental-move-only -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: libdispatch
// UNSUPPORTED: freestanding

// UNSUPPORTED: back_deployment_runtime
// REQUIRES: concurrency_runtime

@preconcurrency import Dispatch

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

final class InlineExecutor: SpecifiedExecutor, CustomStringConvertible {
  let name: String

  init(_ name: String) {
    self.name = name
  }

  public func enqueue(_ job: UnownedJob) {
    print("\(self): enqueue")
    job.runSynchronously(on: self.asUnownedSerialExecutor())
    print("\(self): after run")
  }

  public func enqueue(_ job: __owned Job) {
    print("\(self): enqueue")
    job.runSynchronously(on: self)
    print("\(self): after run")
  }

  var description: Swift.String {
    "InlineExecutor(\(name))"
  }
}

final class NaiveQueueExecutor: SpecifiedExecutor, CustomStringConvertible {
  let queue: DispatchQueue

  init(_ queue: DispatchQueue) {
    self.queue = queue
  }

  public func enqueue(_ job: UnownedJob) {
    print("\(self): enqueue")
    queue.sync {
      job.runSynchronously(on: self)
    }
    print("\(self): after run")
  }

  public func enqueue(_ job: __owned Job) {
    print("\(self): enqueue")
    let unowned = UnownedJob(job)
    queue.sync {
      unowned.runSynchronously(on: self)
    }
    print("\(self): after run")
  }

  var description: Swift.String {
    "NaiveQueueExecutor(\(queue))"
  }
}

actor MyActor: WithSpecifiedExecutor {

  nonisolated let executor: SpecifiedExecutor

  // Note that we don't have to provide the unownedExecutor in the actor itself.
  // We obtain it from the extension on `WithSpecifiedExecutor`.

  init(executor: SpecifiedExecutor) {
    self.executor = executor
  }

  func test(expectedExecutor: some SerialExecutor, expectedQueue: DispatchQueue) {
    precondition(_taskIsOnExecutor(expectedExecutor), "Expected to be on: \(expectedExecutor)")
    dispatchPrecondition(condition: .onQueue(expectedQueue))
    print("\(Self.self): on executor \(expectedExecutor)")
  }
}

@main struct Main {
  static func main() async {
    print("begin")
    let queue = DispatchQueue(label: "CustomQueue")
    let one = NaiveQueueExecutor(queue)
    let actor = MyActor(executor: one)
    await actor.test(expectedExecutor: one, expectedQueue: queue)
    await actor.test(expectedExecutor: one, expectedQueue: queue)
    await actor.test(expectedExecutor: one, expectedQueue: queue)
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
