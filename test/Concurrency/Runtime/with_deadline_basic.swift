// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@available(StdlibDeploymentTarget 6.5, *)
@main struct Main {
  static func main() async {
    await test_returns_before_deadline()
    await test_cancels_operation_when_deadline_expires()
    await test_ambient_task_uncancelled_after_deadline()
    await test_throws_from_operation_before_deadline()
    await test_custom_clock_deadline_subsumption()
    print("done")
  }
}

// Deadline in the far future: operation completes normally, deadline
// never fires, and Task.isCancelled stays false throughout.
@available(StdlibDeploymentTarget 6.5, *)
func test_returns_before_deadline() async {
  print("--- test_returns_before_deadline")
  // CHECK-LABEL: --- test_returns_before_deadline

  let clock = ContinuousClock()
  let deadline = clock.now.advanced(by: .seconds(60))
  let result = try? await withDeadline(deadline, clock: clock) {
    return 42
  }
  print("result=\(result ?? -1)")
  // CHECK: result=42
}

// Short deadline, operation sleeps for a long time. When the deadline
// fires, the private cancellation scope is cancelled, which wakes
// Task.sleep (via the CancellationNotificationStatusRecord installed by
// sleep's withTaskCancellationHandler).
@available(StdlibDeploymentTarget 6.5, *)
func test_cancels_operation_when_deadline_expires() async {
  print("--- test_cancels_operation_when_deadline_expires")
  // CHECK-LABEL: --- test_cancels_operation_when_deadline_expires

  let clock = ContinuousClock()
  let start = clock.now
  let deadline = start.advanced(by: .milliseconds(100))
  do {
    _ = try await withDeadline(deadline, clock: clock) {
      // Attempt to sleep for way past the deadline. Scope cancel should
      // wake this up promptly.
      try await Task.sleep(for: .seconds(30))
    }
    print("returned normally")
  } catch is CancellationError {
    let elapsed = clock.now - start
    if elapsed < .seconds(5) {
      print("threw CancellationError promptly")
      // CHECK: threw CancellationError promptly
    } else {
      print("threw CancellationError but took too long: \(elapsed)")
    }
  } catch {
    print("threw unexpected error: \(error)")
  }
}

// The scope-only cancellation must not touch the enclosing task's own
// isCancelled flag.
@available(StdlibDeploymentTarget 6.5, *)
func test_ambient_task_uncancelled_after_deadline() async {
  print("--- test_ambient_task_uncancelled_after_deadline")
  // CHECK-LABEL: --- test_ambient_task_uncancelled_after_deadline

  await Task {
    let clock = ContinuousClock()
    let deadline = clock.now.advanced(by: .milliseconds(50))
    _ = try? await withDeadline(deadline, clock: clock) {
      try? await Task.sleep(for: .seconds(30))
    }
    print("task after deadline: isCancelled=\(Task.isCancelled)")
    // CHECK: task after deadline: isCancelled=false
  }.value
}

// If the operation throws its own error before the deadline fires, that
// error propagates - the deadline doesn't interfere.
struct MyError: Error {}

@available(StdlibDeploymentTarget 6.5, *)
func test_throws_from_operation_before_deadline() async {
  print("--- test_throws_from_operation_before_deadline")
  // CHECK-LABEL: --- test_throws_from_operation_before_deadline

  let clock = ContinuousClock()
  let deadline = clock.now.advanced(by: .seconds(60))
  do {
    _ = try await withDeadline(deadline, clock: clock) {
      throw MyError()
    }
    print("did not throw")
  } catch is MyError {
    print("threw MyError")
    // CHECK: threw MyError
  } catch {
    print("threw wrong error: \(error)")
  }
}

// Custom clock whose id is a String. Two nested `withDeadline` on the same
// clock instance should have the inner (tighter) deadline observable while
// running inside the inner scope, and the outer deadline observable after
// the inner scope pops. This exercises the tagged-clock-ID path through
// `Builtin.taskPushDeadline` / `Builtin.taskFindNearestDeadlineForClock`.
@available(StdlibDeploymentTarget 6.5, *)
struct StringIdClock: Clock, Identifiable {
  typealias Instant = ContinuousClock.Instant
  typealias Duration = Swift.Duration

  let id: String

  var now: Instant { ContinuousClock.now }
  var minimumResolution: Swift.Duration { .nanoseconds(1) }

  func sleep(until deadline: Instant, tolerance: Swift.Duration?) async throws {
    try await ContinuousClock().sleep(until: deadline, tolerance: tolerance)
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_custom_clock_deadline_subsumption() async {
  print("--- test_custom_clock_deadline_subsumption")
  // CHECK-LABEL: --- test_custom_clock_deadline_subsumption

  let clock = StringIdClock(id: "test-clock")
  // A wide gap between the two deadlines so seconds-level differences are
  // easy to distinguish. The absolute values matter less than the fact
  // that the inner is strictly tighter than the outer.
  let outer = clock.now.advanced(by: .seconds(600))
  let inner = clock.now.advanced(by: .seconds(30))

  _ = try? await withDeadline(outer, clock: clock) {
    let observedOuter = _findNearestDeadline(clock: clock)
    print("outer observed:\(observedOuter != nil)")
    // CHECK: outer observed:true
    let outerSeconds = observedOuter?.seconds ?? -1

    _ = try? await withDeadline(inner, clock: clock) {
      let observedInner = _findNearestDeadline(clock: clock)
      // Inner (tighter) deadline must be strictly less than the outer.
      if let observedInner {
        let isTighter = observedInner.seconds < outerSeconds
        print("inner observed:\(observedInner.seconds < outerSeconds ? "tighter" : "not-tighter")")
        _ = isTighter
        // CHECK: inner observed:tighter
      } else {
        print("inner missing")
      }
    }

    // After the inner scope pops, the outer deadline must be observable again.
    let observedAfter = _findNearestDeadline(clock: clock)
    if let observedAfter {
      let restored = observedAfter.seconds == outerSeconds
      print("after inner pop restored outer:\(restored)")
      // CHECK: after inner pop restored outer:true
    } else {
      print("after inner pop missing")
    }
  }
}

// Matches the "done" line printed at the end of Main.main; must appear as
// the last CHECK in source order because FileCheck matches sequentially.
// CHECK: done
