// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch) | %FileCheck %s
// REQUIRES: concurrency
// REQUIRES: executable_test

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

actor Canceller {
  var hello: String = "checking..."

  func testFunc() async {
    await withTaskCancellationHandler {
      self.assertIsolated("wat in \(#function)!")
      print("testFunc.withTaskCancellationHandler") // CHECK: testFunc.withTaskCancellationHandler
      self.hello = "done!"
    } onCancel: {
      // noop
    }

    // just a simple check to see we executed the closure

    await globalTestFunc()
  }
}
func globalTestFunc(isolation: isolated (any Actor)? = #isolation) async {
  isolation!.assertIsolated("wat in \(#function)!")
  await withTaskCancellationHandler {
     isolation!.assertIsolated("wat in \(#function)!")
     print("globalTestFunc.withTaskCancellationHandler") // CHECK: globalTestFunc.withTaskCancellationHandler
  } onCancel: {
    // noop
  }
}

@MainActor
func testMainActor() async {
  MainActor.preconditionIsolated("Expected main actor")
  await withTaskCancellationHandler {
    MainActor.preconditionIsolated("expected MainActor")
  } onCancel: {
    // noop
  }
}

// FIXME: rdar://155313349 - nonisolated(nonsending) closure does not pick up isolated parameter when the closure is throwing
func testMainActorIsolated(isolation: isolated (any Actor)? = #isolation) async {
  return // FIXME: until rdar://155313349 is fixed
  
  isolation!.preconditionIsolated("Expected main actor")
  MainActor.preconditionIsolated("Expected main actor")
  await withTaskCancellationHandler {
    print("_unsafeInheritExecutor_withTaskCancellationHandler")
    MainActor.preconditionIsolated("expected MainActor")
  } onCancel: {
    // noop
  }
}

_ = await Canceller().testFunc()

_ = await Task { @MainActor in
  await testMainActor()
}.value

_ = await Task { @MainActor in
  await testMainActorIsolated()
}.value

print("done") // CHECK: done