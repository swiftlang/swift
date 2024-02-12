// RUN: %target-run-simple-swift( -Xfrontend -enable-experimental-move-only -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: libdispatch

// rdar://106849189 move-only types should be supported in freestanding mode
// UNSUPPORTED: freestanding

// UNSUPPORTED: back_deployment_runtime
// REQUIRES: concurrency_runtime

import Dispatch
@_spi(ConcurrencyExecutors) import _Concurrency

class BaseExecutor: SerialExecutor, @unchecked Sendable /* only for testing purposes */ {
  let name: String
  let queue: DispatchQueue

  init(name: String, _ queue: DispatchQueue) {
    self.name = name
    self.queue = queue
  }


  func enqueue(_ unowned: UnownedJob) {
    fatalError("not implemented in base class")
  }

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    let ref = UnownedSerialExecutor(complexEquality: self)
    precondition(ref._isComplexEquality, "expected the ref to have complex equality")
    return ref
  }

  func isSameExclusiveExecutionContext(other: BaseExecutor) -> Bool {
    if Set([self.name, other.name]) == Set(["left", "right"]) {
      // those we consider equal
      print("BASE \(BaseExecutor.self).isSameExclusiveExecutionContext: consider \(Self.self)('left') and \(type(of: other))('right') executors as equal context")
      return true
    } else {
      return false
    }
  }
}

final class LeftExecutor: BaseExecutor {
  override init(name: String, _ queue: DispatchQueue) {
    super.init(name: name, queue)
  }

  override func enqueue(_ unowned: UnownedJob) {
    print("\(self): enqueue")
    queue.sync {
      unowned.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }
}

final class RightExecutor: BaseExecutor {
  override init(name: String, _ queue: DispatchQueue) {
    super.init(name: name, queue)
  }

  override func enqueue(_ unowned: UnownedJob) {
    print("\(self): enqueue")
    queue.sync {
      unowned.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }
}

actor MyActor {

  nonisolated let executor: BaseExecutor
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    print("Get executor of \(self): \(executor.asUnownedSerialExecutor())")
    return executor.asUnownedSerialExecutor()
  }

  init(executor: BaseExecutor) {
    self.executor = executor
  }

  func test(expectedExecutor: BaseExecutor, expectedQueue: DispatchQueue) {
    expectedExecutor.preconditionIsolated("Expected deep equality to trigger for \(expectedExecutor) and our \(self.executor)")
    print("\(Self.self): [\(self.executor.name)] on same context as [\(expectedExecutor.name)]")
  }
}

@main struct Main {
  static func main() async {
    print("begin")
    let queue = DispatchQueue(label: "RootQueue")
    let one = LeftExecutor(name: "left", queue)
    let two = RightExecutor(name: "right", queue)
    let actor = MyActor(executor: one)
    await actor.test(expectedExecutor: one, expectedQueue: queue)
    await actor.test(expectedExecutor: two, expectedQueue: queue)
    print("end")
  }
}

// CHECK: begin
// CHECK-NEXT: Get executor of main.MyActor
// CHECK-NEXT: main.LeftExecutor: enqueue
// CHECK-NEXT: MyActor: [left] on same context as [left]
// CHECK-NEXT: BASE BaseExecutor.isSameExclusiveExecutionContext: consider LeftExecutor('left') and RightExecutor('right') executors as equal context
// CHECK-NEXT: MyActor: [left] on same context as [right]
// CHECK-NEXT: end
