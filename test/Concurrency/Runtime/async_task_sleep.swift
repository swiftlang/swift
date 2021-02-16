// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library)
// REQUIRES: executable_test
// REQUIRES: concurrency

import _Concurrency
// FIXME: should not depend on Dispatch
import Dispatch

@main struct Main {
  static let pause = 500_000_000 // 500ms
  
  static func main() async {
    await testSleepDuration()
    await testSleepDoesNotBlock()
  }

  static func testSleepDuration() async {
    let start = DispatchTime.now()

    await Task.sleep(UInt64(pause))

    let stop = DispatchTime.now()

    // assert that at least the specified time passed since calling `sleep`
    assert(stop >= (start + .nanoseconds(pause)))
  }

  static func testSleepDoesNotBlock() async {
    // FIXME: Should run on main executor
    let task = Task.runDetached {
      print("Run first")
    }

    await Task.sleep(UInt64(pause))

    print("Run second")

    await task.get()
  }
}