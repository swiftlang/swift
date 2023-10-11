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
    await testSleepDuration()
    await testSleepDoesNotBlock()
    await testSleepForUIntMax_nanoseconds()
    await testSleepForUIntMax_untilDeadline()
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

  static func testSleepForUIntMax_nanoseconds() async {
    let t = Task.detached {
      print("Run infinite sleep")
      try? await Task.sleep(nanoseconds: .max)
      print("Infinite sleep done (cancelled: \(Task.isCancelled)) @ \(#function)")
      precondition(Task.isCancelled, "Infinite sleep finished yet task was NOT cancelled! @ \(#function)")
    }

    // CHECK: Infinite sleep done (cancelled: true)
    try? await Task.sleep(nanoseconds: 500_000_000) // 500ms
    t.cancel()

    await t.value
  }

  // FIXME: can't quite figure this one out, cc Philippe
  // Uses a different internal API, so we need to test this separately
  static func testSleepForUIntMax_untilDeadline() async {
    let t = Task.detached {
      print("Run infinite sleep")
      try? await Task.sleep(for: .nanoseconds(UInt64.max))
      print("Infinite sleep done (cancelled: \(Task.isCancelled)) @ \(#function)")
      precondition(Task.isCancelled, "Infinite sleep finished yet task was NOT cancelled! @ \(#function)")
    }

    // CHECK: Infinite sleep done (cancelled: true)
    try? await Task.sleep(nanoseconds: 500_000_000) // 500ms
    t.cancel()

    await t.value
  }
}

