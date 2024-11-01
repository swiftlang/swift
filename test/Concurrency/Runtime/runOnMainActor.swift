// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -Xfrontend -disable-dynamic-actor-isolation -swift-version 6 -g -import-objc-header %S/Inputs/RunOnMainActor.h %import-libdispatch )

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// REQUIRES: asserts

// UNSUPPORTED: freestanding

// For now we do not support back deployment or use os stdlib
// UNSUPPORTED: back_deployment_concurrency
// UNSUPPORTED: use_os_stdlib

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

tests.test("checkIfOnMainQueue crashes using pure dispatch queue") { @CustomActor () async -> () in
  expectCrashLater()

  let queue = DispatchQueue(label: "")

  await withCheckedContinuation { cont in
    queue.async {
      checkIfOnMainQueue()
      cont.resume()
    }
  }
}

tests.test("RunOnMainActor on MainActor") { @MainActor () async -> () in
  // This is main actor isolated.
  _taskRunOnMainActor { @MainActor in
    print("=> RunOnMainActor on MainActor")
    checkIfOnMainQueue()
  }
}

tests.test("RunOnMainActor off MainActor") {
  await { @CustomActor in
    _taskRunOnMainActor { @MainActor in
      print("=> RunOnMainActor off MainActor")
      checkIfOnMainQueue()
    }
  }()
}

tests.test("RunOnMainActor off MainActor 2") { @CustomActor () async -> () in
  _taskRunOnMainActor {
    print("=> RunOnMainActor off MainActor 2")
    checkIfOnMainQueue()
  }
}

// This succeeds since assert understands that a child queue runs on the main
// queue.
tests.test("checkIfOnMainQueue in SubQueue from MainActor") { @CustomActor () async -> () in
  let childQueue = DispatchQueue.init(label: "", qos: .background, attributes: [], autoreleaseFrequency: .inherit, target: DispatchQueue.main)

  print("=> checkIfOnMainQueue in SubQueue from MainActor start!")

  // We cannot use the checked continuation, since we are not going to come back
  // to the custom actor.
  await withCheckedContinuation { cont in
    childQueue.async {
      print("=> checkIfOnMainQueue in SubQueue from MainActor")
      checkIfOnMainQueue()
      cont.resume()
    }
  }
}

// This should not fail when backwards deployed since we use the backwards
// compatibility trick.
tests.test("taskRunOnMainActor in SubQueue from MainActor") { @CustomActor () async -> () in
  let childQueue = DispatchQueue.init(label: "", qos: .background, attributes: [], autoreleaseFrequency: .inherit, target: DispatchQueue.main)

  await withCheckedContinuation { cont in
    childQueue.async {
      _taskRunOnMainActor { @MainActor () -> () in
        print("=> taskRunOnMainActor in SubQueue from MainActor")
        checkIfOnMainQueue()
        cont.resume()
      }
    }
  }
}

tests.test("taskRunOnMainActor in SubQueue off MainActor") { @CustomActor () async -> () in
  let d = DispatchQueue.init(label: "", qos: .background)
  let childQueue = DispatchQueue.init(label: "", qos: .background, attributes: [], autoreleaseFrequency: .inherit, target: d)

  await withCheckedContinuation { cont in
    childQueue.async {
      _taskRunOnMainActor { @MainActor in
        print("=> taskRunOnMainActor in SubQueue from MainActor")
        checkIfOnMainQueue()
        cont.resume()
      }
    }
  }
}

await runAllTestsAsync()
