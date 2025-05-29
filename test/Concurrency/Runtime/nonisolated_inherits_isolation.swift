// RUN: %target-run-simple-swift( -swift-version 6 -g %import-libdispatch -import-objc-header %S/Inputs/RunOnMainActor.h -enable-upcoming-feature NonisolatedNonsendingByDefault )

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// REQUIRES: asserts

// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// UNSUPPORTED: freestanding

import StdlibUnittest

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

nonisolated(nonsending)
func executionCallerIsolation() async {
  checkIfOnMainQueue()
}

// Expected to always crash
@concurrent
func executionConcurrentIsolation() async {
  checkIfOnMainQueue()
}

let tests = TestSuite("NonIsolatedInheritsIsolation")

tests.test("checkIfOnMainQueue does not crash on the main queue") { @MainActor () -> () in
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

nonisolated func nonisolatedCheckIfOnMainQueue() async {
  checkIfOnMainQueue()
}

tests.test("Check if nonisolated inherits MainActor does not crash") { @MainActor () async -> () in
  await nonisolatedCheckIfOnMainQueue()
}

tests.test("Check if nonisolated inherits CustomActor crashes") { @CustomActor () async -> () in
  expectCrashLater()
  await nonisolatedCheckIfOnMainQueue()
}

tests.test("Check if nonisolated inheriting nonisolated crashes") { () async -> () in
  // By default this closure is @MainActor isolated. We detach on a background
  // thread and sleep 5 seconds to get the crash. The crash should happen
  // quicker than 5 seconds... we just want to make sure that the code does not
  // exit before we crash.
  expectCrashLater()
  Task.detached {
    await nonisolatedCheckIfOnMainQueue()
  }
  sleep(5)
}

tests.test("Check if execution concurrent isolation crashes (main actor)") { @MainActor () async -> () in
  expectCrashLater()
  await executionConcurrentIsolation()
}

tests.test("Check if execution concurrent isolation crashes (custom actor)") { @CustomActor () async -> () in
  expectCrashLater()
  await executionConcurrentIsolation()
}

tests.test("Check if execution concurrent isolation does not crash (main actor)") { @MainActor () async -> () in
  await executionCallerIsolation()
}

tests.test("Check if execution concurrent isolation does crash (custom actor)") { @CustomActor () async -> () in
  expectCrashLater()
  await executionCallerIsolation()
}

@MainActor func run() async {
  await runAllTestsAsync()
}

await run()
