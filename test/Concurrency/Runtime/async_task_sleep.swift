// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input always
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import _Concurrency
// FIXME: should not depend on Dispatch
import Dispatch

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static let pause = 500_000_000 // 500ms
  
  static func main() async {
//    await testSleepDuration()
//    await testSleepDoesNotBlock()
    await testSleepForUIntMax()
  }

  static func testSleepDuration() async {
    let start = DispatchTime.now()

    await Task.sleep(UInt64(pause))

    let stop = DispatchTime.now()

    // assert that at least the specified time passed since calling `sleep`
    assert(stop >= (start + .nanoseconds(pause)))
  }

  static func testSleepDoesNotBlock() async {
    let task = Task.detached {
      print("Run first")
    }

    await Task.sleep(UInt64(pause))

    print("Run second")

    // CHECK: Run first
    // CHECK: Run second
    await task.get()
  }

  static func testSleepForUIntMax() async {
    let t = Task.detached {
       await Task.sleep(.max)
      print("INFINITE SLEEP DONE (cancelled: \(Task.isCancelled))")
      precondition(Task.isCancelled, "Infinite sleep finished?! And task was NOT cancelled")
    }

    try? await Task.sleep(for: .milliseconds(100))
    t.cancel()

    await t.value
  }
}

