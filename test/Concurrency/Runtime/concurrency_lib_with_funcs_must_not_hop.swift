// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch)
// REQUIRES: concurrency
// REQUIRES: executable_test

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding
// REQUIRES: libdispatch
// REQUIRES: synchronization

import Synchronization

@available(SwiftStdlib 6.0, *)
final class AssertExactEnqueueCountExecutor: SerialExecutor {
  let expectedEnqueueCount: Int
  let enqueueCount: Atomic<Int>

  let name: String
  
  init(expectedEnqueueCount: Int, name: String) {
    self.expectedEnqueueCount = expectedEnqueueCount
    self.enqueueCount = .init(0)
    self.name = name
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    let newEnqueueValue = self.enqueueCount.add(1, ordering: .relaxed).newValue
    if newEnqueueValue > self.expectedEnqueueCount {
      fatalError("Got unexpected enqueue (\(newEnqueueValue)), in: \(self.name)")
    }
    print("[executor][\(self.name)] Enqueue (\(newEnqueueValue))")
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }

  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

@available(SwiftStdlib 6.0, *)
actor TestActor {
  var hello: String = "checking..."
  let executor: AssertExactEnqueueCountExecutor

  init(executor: AssertExactEnqueueCountExecutor) {
    self.executor = executor
  }

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.executor.asUnownedSerialExecutor()
  }

  func testFunc_withTaskCancellationHandler() async {
    await withTaskCancellationHandler {
      self.assertIsolated("wat in \(#function)!")
      print("testFunc.withTaskCancellationHandler")
      self.hello = "done!"
    } onCancel: {
      // noop
    }

    // just a simple check to see we executed the closure
    await globalTestFunc_withTaskCancellationHandler()
  }

  func testFunc_withUnsafeContinuation() async {
    await withUnsafeContinuation { cc in
      self.assertIsolated("wat in \(#function)!")
      print("testFunc.withUnsafeContinuation")
      cc.resume()
    }
  }

  func testFunc_withUnsafeThrowingContinuation() async {
    try! await withUnsafeThrowingContinuation { cc in
      self.assertIsolated("wat in \(#function)!")
      print("testFunc_withUnsafeThrowingContinuation")
      cc.resume()
    }
  }

  func testFunc_withCheckedContinuation() async {
    await withCheckedContinuation { cc in
      self.assertIsolated("wat in \(#function)!")
      print("testFunc_withCheckedContinuation")
      cc.resume()
    }
  }

  func testFunc_withCheckedThrowingContinuation() async {
    try! await withCheckedThrowingContinuation { cc in
      self.assertIsolated("wat in \(#function)!")
      print("withCheckedThrowingContinuation")
      cc.resume()
    }
  }
}

func globalTestFunc_withTaskCancellationHandler(isolation: isolated (any Actor)? = #isolation) async {
  print("\(#function) @ \(#fileID):\(#line)")
  isolation!.assertIsolated("wat in \(#function)!")
  await withTaskCancellationHandler {
    print("\(#function) @ \(#fileID):\(#line)")
    isolation!.assertIsolated("wat in \(#function)!")
    print("globalTestFunc.withTaskCancellationHandler") // CHECK: globalTestFunc.withTaskCancellationHandler
  } onCancel: {
    // noop
  }
}

@MainActor
func testMainActor_withTaskCancellationHandler() async {
  MainActor.preconditionIsolated("Expected main actor")
  await withTaskCancellationHandler {
    MainActor.preconditionIsolated("expected MainActor")
  } onCancel: {
    // noop
  }
}

func testMainActorIsolated_withTaskCancellationHandler(isolation: isolated (any Actor)? = #isolation) async {
  isolation!.preconditionIsolated("Expected main actor")
  MainActor.preconditionIsolated("Expected main actor")
  await withTaskCancellationHandler {
    print("_unsafeInheritExecutor_withTaskCancellationHandler")
    MainActor.preconditionIsolated("expected MainActor")
  } onCancel: {
    // noop
  }
}

if #available(SwiftStdlib 6.0, *) {
  _ = await TestActor(
    executor: .init(
      expectedEnqueueCount: 1,
      name: "testFunc_withTaskCancellationHandler"
    )
  ).testFunc_withTaskCancellationHandler()

  _ = await TestActor(
    executor: .init(
      expectedEnqueueCount: 1,
      name: "testFunc_withUnsafeContinuation"
    )
  ).testFunc_withUnsafeContinuation()

  _ = await TestActor(
    executor: .init(
      expectedEnqueueCount: 1,
      name: "testFunc_withCheckedContinuation"
    )
  ).testFunc_withCheckedContinuation()

  _ = await TestActor(
    executor: .init(
      expectedEnqueueCount: 1,
      name: "testFunc_withUnsafeThrowingContinuation"
    )
  ).testFunc_withUnsafeThrowingContinuation()

  _ = await TestActor(
    executor: .init(
      expectedEnqueueCount: 1,
      name: "testFunc_withCheckedThrowingContinuation"
    )
  ).testFunc_withCheckedThrowingContinuation()
}

_ = await Task { @MainActor in
  await testMainActor_withTaskCancellationHandler()
}.value

_ = await Task { @MainActor in
  await testMainActorIsolated_withTaskCancellationHandler()
}.value

print("done") // CHECK: done