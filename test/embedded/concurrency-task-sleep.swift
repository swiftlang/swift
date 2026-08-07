// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o %target-embedded-posix-shim -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lc++ -lswift_Concurrency %target-swift-default-executor-opt -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1 || OS=emscripten
// REQUIRES: swift_feature_Embedded

import _Concurrency

@main
struct Main {
  static func main() async {
    print("start")
    // CHECK: start
    let clock = ContinuousClock()
    do {
      // 1. Bare nanosecond sleep.
      try await Task.sleep(nanoseconds: 1_000_000)
      print("slept-ns")
      // CHECK-NEXT: slept-ns

      // 2. Duration-based sleep on the default (continuous) clock.
      let beforeFor = clock.now
      try await Task.sleep(for: .milliseconds(50))
      print((clock.now - beforeFor) >= .milliseconds(40) ? "for-ok" : "for-FAST")
      // CHECK-NEXT: for-ok

      // 3. Deadline-based sleep on the continuous clock.
      let beforeUntil = clock.now
      try await Task.sleep(until: .now + .milliseconds(50), clock: .continuous)
      print((clock.now - beforeUntil) >= .milliseconds(40) ? "until-ok" : "until-FAST")
      // CHECK-NEXT: until-ok

      // 4. Deadline-based sleep on the suspending clock.
      let suspendingClock = SuspendingClock()
      let beforeSuspending = suspendingClock.now
      try await Task.sleep(until: .now + .milliseconds(50), clock: .suspending)
      print((suspendingClock.now - beforeSuspending) >= .milliseconds(40)
            ? "suspending-ok" : "suspending-FAST")
      // CHECK-NEXT: suspending-ok

      // 5. Duration-based sleep with an explicit tolerance (exercises the
      //    durationComponents tolerance branch).
      let beforeTolerance = clock.now
      try await Task.sleep(for: .milliseconds(50), tolerance: .milliseconds(10))
      print((clock.now - beforeTolerance) >= .milliseconds(40)
            ? "tolerance-ok" : "tolerance-FAST")
      // CHECK-NEXT: tolerance-ok
    } catch {
      print("error")
    }
    // None of the sleeps above should have thrown.
    // CHECK-NOT: error

    // 6. Cancellation before the sleep starts: must throw promptly.
    let earlyCancel = Task {
      do {
        try await Task.sleep(for: .milliseconds(50))
        print("not-cancelled-early")
      } catch {
        print("cancelled-early")
      }
    }
    earlyCancel.cancel()
    await earlyCancel.value
    // CHECK-NEXT: cancelled-early

    // 7. Cancellation after the sleep is actively enqueued: yield first so the
    //    child reaches its suspension point and enqueues the timer, exercising
    //    the activeContinuation cancel path. It must still throw promptly rather
    //    than wait the full duration.
    let lateCancel = Task {
      do {
        try await Task.sleep(for: .milliseconds(50))
        print("not-cancelled-late")
      } catch {
        print("cancelled-late")
      }
    }
    await Task.yield()
    lateCancel.cancel()
    await lateCancel.value
    // CHECK-NEXT: cancelled-late

    print("done")
    // CHECK-NEXT: done
  }
}
