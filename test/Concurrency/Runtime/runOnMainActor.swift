// RUN: %target-run-simple-swift( -import-objc-header %S/Inputs/RunOnMainActor.h )

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// REQUIRES: asserts

// UNSUPPORTED: freestanding

import StdlibUnittest
import Dispatch

// MARK: Test runOnMainActor in all modes. We use the one future proof API in
// dispatch: dispatch_assert_queue.

////////////////////////
// MARK: Declarations //
////////////////////////

@_silgen_name("dispatch_assert_queue")
func dispatch_assertQueue(_ ptr: UnsafeRawPointer)

func checkIfOnMainQueue() {
  dispatch_assertQueue(getDispatchMain())
}

actor Custom {
}

@globalActor
struct CustomActor {
  static var shared: Custom {
    return Custom()
  }
}

/////////////////
// MARK: Tests //
/////////////////

let tests = TestSuite("RunOnMainActor")

tests.test("RunOnMainActor on MainActor") {
  Task { @MainActor in
    try _runOnMainActor { @MainActor in
      checkIfOnMainQueue()
    }
  }
}

tests.test("RunOnMainActor off MainActor") {
  Task { @CustomActor in
    try _runOnMainActor { @MainActor in
      checkIfOnMainQueue()
    }
  }
}

await runAllTestsAsync()
