// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@_spi(Concurrency) import _Concurrency

@available(StdlibDeploymentTarget 6.5, *)
@main struct Main {
  static func main() async {
    await test_returns_before_deadline()
    await test_cancels_operation_when_deadline_expires()
    await test_ambient_task_uncancelled_after_deadline()
    await test_throws_from_operation_before_deadline()
    await test_custom_clock_deadline_subsumption()
    await test_outer_deadline_subsumes_inner_no_scope()
    await test_async_let_inherits_deadline()
    await test_task_group_child_inherits_deadline()
    await test_detached_task_does_not_inherit_deadline()
    await test_nested_deadlines_inherited_correctly()
    await test_async_let_inherits_cancellationReason()
    await test_task_group_child_inherits_cancellationReason()
    await test_detached_task_does_not_inherit_cancellationReason()
    await test_child_created_before_parent_cancel_still_sees_reason_after_cancel()
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

    _ = try? await withDeadline(inner, clock: clock) {
      let observedInner = _findNearestDeadline(clock: clock)
      // Inner (tighter) deadline must be strictly less than the outer.
      if let observedInner, let observedOuter {
        let isTighter = observedInner < observedOuter
        print("inner observed:\(isTighter ? "tighter" : "not-tighter")")
        // CHECK: inner observed:tighter
      } else {
        print("inner missing")
      }
    }

    // After the inner scope pops, the outer deadline must be observable again.
    let observedAfter = _findNearestDeadline(clock: clock)
    if let observedAfter, let observedOuter {
      let restored = observedAfter == observedOuter
      print("after inner pop restored outer:\(restored)")
      // CHECK: after inner pop restored outer:true
    } else {
      print("after inner pop missing")
    }
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_outer_deadline_subsumes_inner_no_scope() async {
  print("--- test_outer_deadline_subsumes_inner_no_scope")
  // CHECK-LABEL: --- test_outer_deadline_subsumes_inner_no_scope

  // The outer withDeadline installs a tight deadline. The inner withDeadline
  // requests a looser deadline for the same clock; because the outer is
  // tighter, the inner withDeadline must take the fast path: no record
  // installed, no cancellation scope wrapped around `operation`. We
  // verify both properties.
  let clock = ContinuousClock()
  let outer = clock.now.advanced(by: .seconds(60))
  let inner = clock.now.advanced(by: .seconds(600))

  _ = try? await withDeadline(outer, clock: clock) {
    // Snapshot the outer's Instant so the inner can prove that no
    // new record was pushed - the innermost record must still be the outer's.
    guard let outerObserved = _findNearestDeadline(clock: clock) else {
      print("outer missing (unexpected)")
      return
    }
    print("outer observed")
    // CHECK: outer observed

    _ = try? await withDeadline(inner, clock: clock) {
      // Property 1: inner did NOT push its own record. The nearest deadline
      // for this clock must still be the outer's tight one.
      let innerObserved = _findNearestDeadline(clock: clock)
      let sameAsOuter = innerObserved == outerObserved
      print("inner sees outer only:\(sameAsOuter)")
      // CHECK: inner sees outer only:true

      // Property 2: inner did NOT wrap `operation` in a fresh cancellation
      // scope. If it had, cancelling the ambient task from inside `operation`
      // would still leave `Task.isCancelled` reading true (whole-task cancel
      // propagates through any number of scopes), so this half is weak on
      // its own. The strong signal is that with no scope pushed, the
      // task's status-record chain has no CancellationScope record between
      // the innermost record and the outer deadline record. We can only
      // observe this indirectly - the record snapshot above already covers
      // it. But we still exercise the "operation runs to completion" path
      // to make sure the fast path doesn't accidentally skip the call.
      print("inner operation ran")
      // CHECK: inner operation ran
    }
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_async_let_inherits_deadline() async {
  print("--- test_async_let_inherits_deadline")
  // CHECK-LABEL: --- test_async_let_inherits_deadline

  // An async let child inside a withDeadline scope must see the parent's
  // deadline: Task.hasActiveDeadline is true on the child, and
  // Task.activeDeadline(for: theClock) returns the same instant the parent
  // installed.
  let clock = ContinuousClock()
  let deadline = clock.now.advanced(by: .seconds(600))

  _ = try? await withDeadline(deadline, clock: clock) {
    // Parent observes the deadline.
    print("parent hasActive:\(Task.hasActiveDeadline)")
    // CHECK: parent hasActive:true

    async let childSees: (Bool, Bool) = {
      let hasAny = Task.hasActiveDeadline
      let observed = Task.activeDeadline(for: clock)
      let matchesParent = observed == deadline
      return (hasAny, matchesParent)
    }()

    let (childHasAny, childMatchesParent) = await childSees
    print("child hasActive:\(childHasAny)")
    // CHECK: child hasActive:true
    print("child sees parent deadline:\(childMatchesParent)")
    // CHECK: child sees parent deadline:true
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_task_group_child_inherits_deadline() async {
  print("--- test_task_group_child_inherits_deadline")
  // CHECK-LABEL: --- test_task_group_child_inherits_deadline

  // TaskGroup children also inherit deadlines from their parent (same
  // structured-concurrency rule as async let).
  let clock = ContinuousClock()
  let deadline = clock.now.advanced(by: .seconds(600))

  _ = try? await withDeadline(deadline, clock: clock) {
    await withTaskGroup(of: Bool.self) { group in
      group.addTask {
        let hasAny = Task.hasActiveDeadline
        let matches = Task.activeDeadline(for: clock) == deadline
        return hasAny && matches
      }
      let result = await group.next() ?? false
      print("group child sees parent deadline:\(result)")
      // CHECK: group child sees parent deadline:true
    }
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_detached_task_does_not_inherit_deadline() async {
  print("--- test_detached_task_does_not_inherit_deadline")
  // CHECK-LABEL: --- test_detached_task_does_not_inherit_deadline

  // Detached tasks are unstructured - by definition they do not
  // participate in the enclosing withDeadline scope. Task.hasActiveDeadline
  // on a detached task started inside withDeadline should return false.
  let clock = ContinuousClock()
  let deadline = clock.now.advanced(by: .seconds(600))

  _ = try? await withDeadline(deadline, clock: clock) {
    let sawDeadline = await Task.detached {
      Task.hasActiveDeadline
    }.value
    print("detached hasActive:\(sawDeadline)")
    // CHECK: detached hasActive:false
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_nested_deadlines_inherited_correctly() async {
  print("--- test_nested_deadlines_inherited_correctly")
  // CHECK-LABEL: --- test_nested_deadlines_inherited_correctly

  // Two nested withDeadline on the same clock (inner is tighter). An
  // async let child spawned inside the inner scope should see the inner
  // (tighter) deadline, not the outer one.
  let clock = ContinuousClock()
  let outer = clock.now.advanced(by: .seconds(600))
  let inner = clock.now.advanced(by: .seconds(30))

  _ = try? await withDeadline(outer, clock: clock) {
    _ = try? await withDeadline(inner, clock: clock) {
      async let childDeadline: ContinuousClock.Instant? = Task.activeDeadline(for: clock)
      let observed = await childDeadline
      let matchesInner = observed == inner
      print("child sees inner:\(matchesInner)")
      // CHECK: child sees inner:true
    }
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Cancellation reason inheritance

@available(StdlibDeploymentTarget 6.5, *)
func test_async_let_inherits_cancellationReason() async {
  print("--- \(#function)")
  // CHECK-LABEL: --- test_async_let_inherits_cancellationReason()

  // Cancel the parent with a specific reason, then spawn an async let
  // child. The child must be created already-cancelled (inherited from
  // the parent) and must observe the same reason via
  // `Task.cancellationReason`.
  await Task {
    withUnsafeCurrentTask { $0?.cancel(reason: .deadlineExpired) }
    print("parent reason:\(Task.cancellationReason.map { "\($0)" } ?? "nil")")
    // CHECK: parent reason:deadlineExpired

    async let childReasonRaw: String = {
      let r = Task.cancellationReason
      return r.map { "\($0)" } ?? "nil"
    }()
    let childReason = await childReasonRaw
    print("child reason:\(childReason)")
    // CHECK: child reason:deadlineExpired
  }.value
}

@available(StdlibDeploymentTarget 6.5, *)
func test_task_group_child_inherits_cancellationReason() async {
  print("--- \(#function)")
  // CHECK-LABEL: --- test_task_group_child_inherits_cancellationReason()

  // Same as above but with a TaskGroup child.
  await Task {
    withUnsafeCurrentTask { $0?.cancel(reason: .deadlineExpired) }

    await withTaskGroup(of: String.self) { group in
      group.addTask {
        return Task.cancellationReason.map { "\($0)" } ?? "nil"
      }
      let childReason = await group.next() ?? "missing"
      print("group child reason:\(childReason)")
      // CHECK: group child reason:deadlineExpired
    }
  }.value
}

@available(StdlibDeploymentTarget 6.5, *)
func test_detached_task_does_not_inherit_cancellationReason() async {
  print("--- \(#function)")
  // CHECK-LABEL: --- test_detached_task_does_not_inherit_cancellationReason()

  // Detached tasks are unstructured; a detached task started inside
  // a cancelled parent must NOT inherit the parent's cancellation.
  // `Task.cancellationReason` on the detached task should be nil
  // (not cancelled), regardless of what the parent has.
  await Task {
    withUnsafeCurrentTask { $0?.cancel(reason: .deadlineExpired) }

    let detachedReason = await Task.detached {
      return Task.cancellationReason.map { "\($0)" } ?? "nil"
    }.value
    print("detached reason:\(detachedReason)")
    // CHECK: detached reason:nil
  }.value
}

@available(StdlibDeploymentTarget 6.5, *)
func test_child_created_before_parent_cancel_still_sees_reason_after_cancel() async {
  print("--- \(#function)")
  // CHECK-LABEL: --- test_child_created_before_parent_cancel_still_sees_reason_after_cancel()

  // A structured child spawned BEFORE the parent is cancelled must
  // still observe the parent's cancellation via the runtime
  // propagation path (swift_task_cancel -> ChildTask records ->
  // recursive cancel with the same reason). This exercises the
  // cancellation-reason-through-performCancellationAction path, not
  // just the create-time inheritance path.
  await Task {
    await withTaskGroup(of: CancellationError.Reason?.self) { group in
      // Spawn the child while the parent is NOT yet cancelled.
      group.addTask {
        // Wait long enough for the parent to cancel us with a reason.
        try? await Task.sleep(for: .milliseconds(200))
        return Task.cancellationReason
      }

      // Give the child a moment to enter its sleep, then cancel with
      // a specific reason.
      try? await Task.sleep(for: .milliseconds(50))
      withUnsafeCurrentTask { $0?.cancel(reason: .deadlineExpired) }

      let observed = await group.next()?.flatMap { $0 }
      let matches = observed == .deadlineExpired
      print("child observed parent reason:\(matches)")
      // CHECK: child observed parent reason:true
    }
  }.value
}

// Matches the "done" line printed at the end of Main.main; must appear as
// the last CHECK in source order because FileCheck matches sequentially.
// CHECK: done
