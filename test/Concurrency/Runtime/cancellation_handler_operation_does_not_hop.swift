// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -target %target-swift-5.1-abi-triple %import-libdispatch) | %FileCheck %s
// REQUIRES: concurrency
// REQUIRES: executable_test

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

actor Canceller {
  func testFunc() async {
    await withTaskCancellationHandler {
      self.assertIsolated("wat in \(#function)!")
    } onCancel: {
      // noop
    }

    await globalTestFunc()
  }
}
func globalTestFunc(isolation: isolated (any Actor)? = #isolation) async {
  isolation!.assertIsolated("wat in \(#function)!")
  await withTaskCancellationHandler {
     isolation!.assertIsolated("wat in \(#function)!")
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

func testMainActorIsolated(isolation: isolated (any Actor)? = #isolation) async {
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
