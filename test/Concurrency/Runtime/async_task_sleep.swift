// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input always
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
    await testSleepHuge()
    if #available(SwiftStdlib 5.7, *) {
      await testSleepNegative()
    }
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
    let task = Task.detached {
      print("Run first")
    }

    await Task.sleep(UInt64(pause))

    print("Run second")

    // CHECK: Run first
    // CHECK: Run second
    await task.get()
  }

  static func testSleepHuge() async {
    // Make sure nanoseconds values about Int64.max don't get interpreted as
    // negative and fail to sleep.
    let task1 = Task.detached {
      try await Task.sleep(nanoseconds: UInt64(Int64.max) + 1)
    }
    let task2 = Task.detached {
      try await Task.sleep(nanoseconds: UInt64.max)
    }

    try! await Task.sleep(nanoseconds: UInt64(pause))

    task1.cancel()
    task2.cancel()

    // These should throw due to being canceled. If the sleeps completed then
    // the cancellation will do nothing and we won't throw, which is a failure.
    do {
      _ = try await task1.value
      fatalError("Sleep 1 completed early.")
    } catch {}
    do {
      _ = try await task2.value
      fatalError("Sleep 2 completed early.")
    } catch {}
  }

  @available(SwiftStdlib 5.7, *)
  static func testSleepNegative() async {
    // Make sure that "negative" times don't cause us to sleep forever
    let negativeDuration = Duration(secondsComponent: -60,
                                    attosecondsComponent: 0)
    let negativeTime = unsafe unsafeBitCast(negativeDuration,
                                            to: ContinuousClock.Instant.self)

    let task = Task.detached {
      try await Task.sleep(until: negativeTime)
    }

    try! await Task.sleep(nanoseconds: UInt64(pause))

    task.cancel()

    // The sleep should complete; if they throw, this is a failure.
    do {
      _ = try await task.value
    } catch {
      fatalError("Sleep tried to wait for a negative time")
    }
  }
}
