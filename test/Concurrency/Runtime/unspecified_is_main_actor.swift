// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -swift-version 6 -g -import-objc-header %S/Inputs/RunOnMainActor.h %import-libdispatch -Xfrontend -default-isolation=MainActor )

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// REQUIRES: asserts

// UNSUPPORTED: freestanding

// For now we do not support back deployment or use os stdlib
// UNSUPPORTED: back_deployment_concurrency
// UNSUPPORTED: use_os_stdlib

// Just a runtime test as a sanity check.

import StdlibUnittest
import Dispatch

////////////////////////
// MARK: Declarations //
////////////////////////

@_silgen_name("dispatch_assert_queue")
nonisolated func dispatch_assertQueue(_ ptr: UnsafeRawPointer)

nonisolated func checkIfOnMainQueue() {
  dispatch_assertQueue(getDispatchMain())
}

actor Custom {
}

@globalActor
struct CustomActor {
  static nonisolated var shared: Custom {
    return Custom()
  }
}

///////////////////////////////////////////
// MARK: Scaffolding/Testing Scaffolding //
///////////////////////////////////////////

let tests = TestSuite("UnspecifiedIsMainActor")

tests.test("checkIfOnMainQueue does not crash on the main queue") { @MainActor () -> () in
  // Why do we crash if this is synchronous.
  expectCrashLater()
  checkIfOnMainQueue()
}

tests.test("checkIfOnMainQueue does not crash on the main queue") { @MainActor () async -> () in
  checkIfOnMainQueue()
}

tests.test("checkIfOnMainQueue crashes off the main queue") {
  expectCrashLater()
  await { @CustomActor in
    print("=> checkIfOnMainQueue crashes off the main queue")
    checkIfOnMainQueue()
  }()
}

tests.test("checkIfOnMainQueue crashes off the main queue 2") { @CustomActor () async -> () in
  expectCrashLater()
  print("=> checkIfOnMainQueue crashes off the main queue 2")
  checkIfOnMainQueue()
}

/////////////////
// MARK: Tests //
/////////////////

class Klass {}

struct MainActorIsolated {
  init() {
    checkIfOnMainQueue()
  }

  func test() async {
    checkIfOnMainQueue()
  }
};

tests.test("callNominalType") { @CustomActor () -> () in
  let x = await MainActorIsolated()
  // We would crash without hopping here.
  await x.test()
}

await runAllTestsAsync()
