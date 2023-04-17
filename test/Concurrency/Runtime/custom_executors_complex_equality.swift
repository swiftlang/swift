// RUN: %target-run-simple-swift( -Xfrontend -enable-experimental-move-only -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: libdispatch

// rdar://106849189 move-only types should be supported in freestanding mode
// UNSUPPORTED: freestanding

// UNSUPPORTED: back_deployment_runtime
// REQUIRES: concurrency_runtime

@preconcurrency import Dispatch
@_spi(ConcurrencyExecutors) import _Concurrency

final class NaiveQueueExecutor: SerialExecutor, CustomStringConvertible {
  let name: String
  let queue: DispatchQueue

  init(name: String, _ queue: DispatchQueue) {
    self.name = name
    self.queue = queue
  }

  public func enqueue(_ unowned: UnownedJob) {
    print("\(self): enqueue")
    queue.sync {
      unowned.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }

  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    let ref = UnownedSerialExecutor(complexEquality: self)
    precondition(ref._isComplexEquality, "expected the ref to have complex equality")
    return ref
  }

  public func isSameExclusiveExecutionContext(other: NaiveQueueExecutor) -> Bool {
    if Set([self.name, other.name]) == Set(["one", "two"]) {
      // those we consider equal
      print("isSameExclusiveExecutionContext: consider 'one' and 'two' executors as equal context")
      return true
    } else {
      return false
    }
  }

  var description: Swift.String {
    "NaiveQueueExecutor(\(name), \(queue))"
  }
}

actor MyActor {

  nonisolated let executor: NaiveQueueExecutor
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    print("Get executor of \(self): \(executor.asUnownedSerialExecutor())")
    return executor.asUnownedSerialExecutor()
  }

  init(executor: NaiveQueueExecutor) {
    self.executor = executor
  }

  func test(expectedExecutor: NaiveQueueExecutor, expectedQueue: DispatchQueue) {
    expectedExecutor.preconditionIsolated("Expected deep equality to trigger for \(expectedExecutor) and our \(self.executor)")
    print("\(Self.self): [\(self.executor.name)] on same context as [\(expectedExecutor.name)]")
  }
}

@main struct Main {
  static func main() async {
    print("begin")
    let queue = DispatchQueue(label: "RootQueue")
    let one = NaiveQueueExecutor(name: "one", queue)
    let two = NaiveQueueExecutor(name: "two", queue)
    let actor = MyActor(executor: one)
    await actor.test(expectedExecutor: one, expectedQueue: queue)
    await actor.test(expectedExecutor: two, expectedQueue: queue)
    print("end")
  }
}

// CHECK:      begin
// CHECK-NEXT: Get executor of main.MyActor: UnownedSerialExecutor(executor:
// CHECK-NEXT: NaiveQueueExecutor(one,
// CHECK-NEXT: MyActor: [one] on same context as [one]
// CHECK-NEXT: isSameExclusiveExecutionContext: consider 'one' and 'two' executors as equal context
// CHECK-NEXT: MyActor: [one] on same context as [two]
// CHECK-NEXT: end
