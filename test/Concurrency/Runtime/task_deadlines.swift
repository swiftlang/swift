// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@_spi(Concurrency) import _Concurrency
import StdlibUnittest
import Synchronization

struct MyError: Error {}

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

// A class type used as an Instant payload to verify that the record's tail
// storage participates correctly in ARC: push retains +1 into the record,
// pop drops that +1, and `_findNearestDeadline` returns a properly-owned
// copy (loading the reference through `.pointee` bumps the refcount).
@available(StdlibDeploymentTarget 6.5, *)
final class InstantBox: @unchecked Sendable {
  let underlying: ContinuousClock.Instant
  static var liveCount: Int = 0
  init(_ underlying: ContinuousClock.Instant) {
    self.underlying = underlying
    InstantBox.liveCount += 1
  }
  deinit {
    InstantBox.liveCount -= 1
  }
}

// A class wrapping an `Atomic<Int>` so an escaping `onCancel` closure can
// capture a copyable class reference instead of the non-copyable atomic by
// value. Capturing the `Atomic<Int>` directly crashes the SIL ownership
// verifier on the -enable-sil-opaque-values path
final class CounterBox: @unchecked Sendable {
  let value = Atomic<Int>(0)
}

@available(StdlibDeploymentTarget 6.5, *)
struct ClassInstantClock: Clock, Identifiable {
  typealias Duration = Swift.Duration
  struct Instant: InstantProtocol {
    let box: InstantBox
    typealias Duration = Swift.Duration
    static func < (a: Instant, b: Instant) -> Bool { a.box.underlying < b.box.underlying }
    static func == (a: Instant, b: Instant) -> Bool { a.box.underlying == b.box.underlying }
    func hash(into hasher: inout Hasher) { hasher.combine(box.underlying) }
    func advanced(by d: Duration) -> Instant { Instant(box: InstantBox(box.underlying.advanced(by: d))) }
    func duration(to other: Instant) -> Duration { box.underlying.duration(to: other.box.underlying) }
  }
  let id: String
  var now: Instant { Instant(box: InstantBox(ContinuousClock.now)) }
  var minimumResolution: Swift.Duration { .nanoseconds(1) }
  func sleep(until deadline: Instant, tolerance: Swift.Duration?) async throws {
    try await ContinuousClock().sleep(until: deadline.box.underlying, tolerance: tolerance)
  }
}

@available(StdlibDeploymentTarget 6.5, *)
@main struct Main {
  static func main() async {
    let tests = TestSuite("TaskDeadlines")

    tests.test("returns before deadline") {
      let result = try? await withDeadline(in: .seconds(60)) {
        return 42
      }
      expectEqual(42, result)
    }

    tests.test("cancels operation when deadline expires") {
      do {
        _ = try await withDeadline(in: .milliseconds(100)) {
          // Attempt to sleep for way past the deadline.
          // Deadline-caused cancel should wake this up promptly.
          try await Task.sleep(for: .seconds(30))
        }
        expectUnreachable("withDeadline should have thrown CancellationError")
      } catch is CancellationError {
        // expected, good!
      } catch {
        expectUnreachableCatch(error)
      }
    }

    tests.test("past deadline still runs operation") {
      // Body that ignores cancellation
      let ignoringResult = try? await withDeadline(in: .seconds(-60)) {
        return 42
      }
      expectEqual(42, ignoringResult)

      // Body that checks isCancelled
      let checkingResult: Int? = try? await withDeadline(in: .seconds(-60)) { () -> Int in
        let observedCancelled = Task.isCancelled
        expectTrue(observedCancelled)
        if observedCancelled { return 7 }
        return 99
      }
      expectEqual(7, checkingResult)

      // Body that checks cancellationReason
      let reasonFromInside = try? await withDeadline(in: .seconds(-60)) { () -> CancellationError.Reason? in
        return Task.cancellationReason
      }
      expectEqual(.deadlineExpired, reasonFromInside.flatMap { $0 })
      expectEqual(nil, Task.cancellationReason)

      // Body that calls checkCancellation()
      do {
        _ = try await withDeadline(in: .seconds(-60)) { () -> Int in
          try Task.checkCancellation()
          return 99
        }
        expectUnreachable("checkCancellation() should have thrown")
      } catch let error as CancellationError {
        expectEqual(.deadlineExpired, error.reason)
      } catch {
        expectUnreachableCatch(error)
      }
    }

    tests.test("surrounding task uncancelled after deadline") {
      await Task {
        let innerCancelled = try? await withDeadline(in: .milliseconds(50)) {
          try? await Task.sleep(for: .seconds(30))
          return Task.isCancelled
        }
        expectEqual(true, innerCancelled)
        expectFalse(Task.isCancelled)
      }.value
    }

    tests.test("throws from operation before deadline") {
      do {
        _ = try await withDeadline(in: .seconds(60)) {
          throw MyError()
        }
        expectUnreachable("withDeadline should have thrown MyError")
      } catch is MyError {
        // Expected.
      } catch {
        expectUnreachableCatch(error)
      }
    }

    tests.test("custom clock deadline subsumption") {
      let clock = StringIdClock(id: "test-clock")
      // A wide gap between the two deadlines so seconds-level differences are
      // easy to distinguish. The absolute values matter less than the fact
      // that the inner is strictly tighter than the outer.
      let outer = clock.now.advanced(by: .seconds(600))
      let inner = clock.now.advanced(by: .seconds(30))

      _ = try? await withDeadline(outer, clock: clock) {
        let observedOuter = _findNearestDeadline(clock: clock)
        expectNotNil(observedOuter)

        _ = try? await withDeadline(inner, clock: clock) {
          let observedInner = _findNearestDeadline(clock: clock)
          // Inner (tighter) deadline must be strictly less than the outer.
          if let observedInner, let observedOuter {
            expectTrue(observedInner < observedOuter)
          } else {
            expectUnreachable("inner deadline missing")
          }
        }

        // After the inner scope pops, the outer deadline must be observable again.
        let observedAfter = _findNearestDeadline(clock: clock)
        if let observedAfter, let observedOuter {
          expectEqual(observedOuter, observedAfter)
        } else {
          expectUnreachable("outer deadline missing after inner pop")
        }
      }
    }

    tests.test("class instant lifetime") {
      InstantBox.liveCount = 0
      let clock = ClassInstantClock(id: "class-instant")
      let deadline = clock.now.advanced(by: .seconds(600))

      _ = try? await withDeadline(deadline, clock: clock) {
        // The record's copy of the instant must be live here.
        guard let observed = _findNearestDeadline(clock: clock) else {
          expectUnreachable("no deadline observed")
          return
        }
        // Loading through `.pointee` returned an owned copy: the underlying box
        // pointer must be a live object equal by refcount rules to the original.
        expectTrue(InstantBox.liveCount > 0)
        _ = observed
      }

      // After `withDeadline` returns, its own local `deadline` still owns +1;
      // the record's +1 was released by pop. Drop `deadline` and confirm the
      // box goes away.
      withExtendedLifetime(deadline) {}
    }

    tests.test("outer deadline subsumes inner, no scope") {
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
          expectUnreachable("outer deadline missing")
          return
        }

        var innerOperationRan = false
        _ = try? await withDeadline(inner, clock: clock) {
          // Inner did NOT push its own record. The nearest deadline
          // for this clock must still be the outer one.
          let innerObserved = _findNearestDeadline(clock: clock)
          expectEqual(outerObserved, innerObserved)

          // Inner did NOT wrap `operation` in a fresh cancellation
          // scope. If it had, cancelling the ambient task from inside `operation`
          // would still leave `Task.isCancelled` reading true (whole-task cancel
          // propagates through any number of scopes), so this half is weak on
          // its own. The strong signal is that with no scope pushed, the
          // task's status-record chain has no CancellationScope record between
          // the innermost record and the outer deadline record. We can only
          // observe this indirectly - the record snapshot above already covers
          // it. But we still exercise the "operation runs to completion" path
          // to make sure the fast path doesn't accidentally skip the call.
          innerOperationRan = true
        }
        expectTrue(innerOperationRan)
      }
    }

    tests.test("async let inherits deadline") {
      let clock = ContinuousClock()
      let deadline = clock.now.advanced(by: .seconds(600))

      _ = try? await withDeadline(deadline, clock: clock) {
        // Parent observes the deadline.
        expectTrue(Task.hasActiveDeadline)

        async let childSees: (Bool, Bool) = {
          let hasAny = Task.hasActiveDeadline
          let observed = Task.activeDeadline(for: clock)
          let matchesParent = observed == deadline
          return (hasAny, matchesParent)
        }()

        let (childHasAny, childMatchesParent) = await childSees
        expectTrue(childHasAny)
        expectTrue(childMatchesParent)
      }
    }

    tests.test("task group child inherits deadline") {
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
          expectTrue(result)
        }
      }
    }

    // Detached tasks are unstructured - by definition they do not
    // participate in the enclosing withDeadline scope. Task.hasActiveDeadline
    // on a detached task started inside withDeadline should return false.
    tests.test("detached task does not inherit deadline") {
      _ = try? await withDeadline(in: .seconds(600)) {
        let sawDeadline = await Task.detached {
          Task.hasActiveDeadline
        }.value
        expectFalse(sawDeadline)
      }
    }

    // Two nested withDeadline on the same clock (inner is tighter). An
    // async let child spawned inside the inner scope should see the inner
    // (tighter) deadline, not the outer one.
    tests.test("nested deadlines inherited correctly") {
      let clock = ContinuousClock()
      let outer = clock.now.advanced(by: .seconds(600))
      let inner = clock.now.advanced(by: .seconds(30))

      _ = try? await withDeadline(outer, clock: clock) {
        _ = try? await withDeadline(inner, clock: clock) {
          async let childDeadline: ContinuousClock.Instant? = Task.activeDeadline(for: clock)
          let observed = await childDeadline
          expectEqual(inner, observed)
        }
      }
    }

    // ==== ---------------------------------------------------------------
    // MARK: Cancellation reason inheritance

    // Cancel the parent with a specific reason, then spawn an async let
    // child. The child must be created already-cancelled (inherited from
    // the parent) and must observe the same reason via
    // `Task.cancellationReason`.
    tests.test("async let inherits cancellationReason") {
      await Task {
        withUnsafeCurrentTask { $0?.cancel(reason: .deadlineExpired) }
        expectEqual(.deadlineExpired, Task.cancellationReason)

        async let childReason: CancellationError.Reason? = Task.cancellationReason
        expectEqual(.deadlineExpired, await childReason)
      }.value
    }

    // Same as above but with a TaskGroup child.
    tests.test("task group child inherits cancellationReason") {
      await Task {
        withUnsafeCurrentTask { $0?.cancel(reason: .deadlineExpired) }

        await withTaskGroup(of: CancellationError.Reason?.self) { group in
          group.addTask {
            return Task.cancellationReason
          }
          let childReason = await group.next() ?? nil
          expectEqual(.deadlineExpired, childReason)
        }
      }.value
    }

    // Detached tasks are unstructured; a detached task started inside
    // a cancelled parent must NOT inherit the parent's cancellation.
    // `Task.cancellationReason` on the detached task should be nil
    // (not cancelled), regardless of what the parent has.
    tests.test("detached task does not inherit cancellationReason") {
      await Task {
        withUnsafeCurrentTask { $0?.cancel(reason: .deadlineExpired) }

        let detachedReason = await Task.detached {
          return Task.cancellationReason
        }.value
        expectNil(detachedReason)
      }.value
    }

    // A structured child spawned BEFORE the parent is cancelled must
    // still observe the parent's cancellation via the runtime
    // propagation path (swift_task_cancel -> ChildTask records ->
    // recursive cancel with the same reason). This exercises the
    // cancellation-reason-through-performCancellationAction path, not
    // just the create-time inheritance path.
    tests.test("child created before parent cancel still sees reason after cancel") {
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
          expectEqual(.deadlineExpired, observed)
        }
      }.value
    }

    // ==== ---------------------------------------------------------------
    // MARK: Nested deadlines (proposal Examples 2-5)

    // Example 2: inner deadline is tighter than outer. The inner handler fires
    // (the inner cancellation scope is cancelled when the inner deadline
    // expires); the outer handler does NOT fire because the outer scope was
    // never cancelled. When `operation` inside inner throws its own error,
    // that error propagates out unchanged.
    tests.test("nested deadline, inner tighter, operation error propagates") {
      var outerHandlerCount = 0
      var innerHandlerCount = 0

      do {
        try await withTaskCancellationHandler {
          _ = try await withDeadline(in: .seconds(10)) {
            try await withTaskCancellationHandler {
              try await withDeadline(in: .milliseconds(100)) {
                throw MyError()
              }
            } onCancel: {
              innerHandlerCount += 1
            }
          }
        } onCancel: {
          outerHandlerCount += 1
        }
        expectUnreachable("withDeadline should have thrown MyError")
      } catch is MyError {
        // Expected.
      } catch {
        expectUnreachableCatch(error)
      }

      // The inner deadline never fired (operation threw first), so we expect
      // BOTH handler counts to be 0 here. This documents that a throw beats
      // a deadline: neither the inner nor the outer scope was cancelled.
      expectEqual(0, outerHandlerCount)
      expectEqual(0, innerHandlerCount)
    }

    // Example 3: outer deadline is tighter than inner. The outer deadline
    // fires first, cancels its own scope, which propagates inward: every
    // CancellationNotificationStatusRecord installed inside the outer scope's
    // dynamic extent fires. Both the outer-scope handler and the inner-scope
    // handler live inside the outer scope, so BOTH must fire. (Handlers
    // installed OUTSIDE the outer `withDeadline` would NOT fire - see
    // `test_scope_handler_outside_does_not_fire_on_scope_cancel` in
    // task_cancellation_scope.swift for the counterpart property.)
    tests.test("nested deadline, outer tighter, both handlers fire") {
      let clock = ContinuousClock()
      let start = clock.now

      var outerHandlerCount = 0
      var innerHandlerCount = 0

      _ = try? await withDeadline(in: .milliseconds(100)) {
        try await withTaskCancellationHandler {
          try await withDeadline(in: .seconds(10)) {
            try await withTaskCancellationHandler {
              try await Task.sleep(for: .seconds(30))
            } onCancel: {
              innerHandlerCount += 1
            }
          }
        } onCancel: {
          outerHandlerCount += 1
        }
      }

      let elapsed = clock.now - start
      expectTrue(elapsed < .seconds(5))
      expectEqual(1, outerHandlerCount)
      expectEqual(1, innerHandlerCount)
    }

    // Same nesting shape as before, but the inner `sleep` is
    // shorter than the inner deadline. Outer is still the tighter deadline, so
    // both handlers still fire when outer fires (and the inner sleep never
    // gets to return on its own).
    tests.test("nested deadline, outer tighter, inner short sleep") {
      let clock = ContinuousClock()
      let start = clock.now

      var outerHandlerCount = 0
      var innerHandlerCount = 0

      _ = try? await withDeadline(in: .milliseconds(100)) {
        try await withTaskCancellationHandler {
          try await withDeadline(in: .seconds(10)) {
            try await withTaskCancellationHandler {
              try await Task.sleep(for: .seconds(5))
            } onCancel: {
              innerHandlerCount += 1
            }
          }
        } onCancel: {
          outerHandlerCount += 1
        }
      }

      let elapsed = clock.now - start
      expectTrue(elapsed < .seconds(2))
      expectEqual(1, outerHandlerCount)
      expectEqual(1, innerHandlerCount)
    }
    
    tests.test("outer cancellation handler is unaffected by nested deadline scopes cancelling locally") {
      // The counter is wrapped in a class so the escaping `onCancel` closure
      // captures a copyable class reference rather than the non-copyable
      // `Atomic<Int>` by value, which trips the SIL ownership verifier on the
      // -enable-sil-opaque-values path
      let handlerCount = CounterBox()
      do {
        try await withTaskCancellationHandler {
          try await withDeadline(in: .seconds(-60)) {
            expectTrue(Task.isCancelled)
          }
          try await withDeadline(in: .seconds(-60)) {
            expectTrue(Task.isCancelled)
          }
        } onCancel: {
          handlerCount.value.wrappingAdd(1, ordering: .relaxed)
        }
      } catch {
        expectUnreachableCatch(error)
      }
      expectEqual(0, handlerCount.value.load(ordering: .relaxed))
    }

    final class ContinuationBox: @unchecked Sendable {
      var continuation: CheckedContinuation<Void, Never>?
    }
    tests.test("cancellation handler inside a scope fires only once even if the scope and the whole task both cancel") {
      let handlerCount = CounterBox()
      let box = ContinuationBox()
      let task = Task {
        _ = try? await withDeadline(in: .milliseconds(50)) {
          try? await withTaskCancellationHandler {
            await withCheckedContinuation { (continuation: CheckedContinuation<Void, Never>) in
              box.continuation = continuation
            }
          } onCancel: {
            handlerCount.value.wrappingAdd(1, ordering: .relaxed)
          }
        }
      }
      // Let the deadline (50ms) elapse, firing the scope-cancel walk.
      try? await Task.sleep(for: .milliseconds(150))
      // The handler record is still installed (`operation` is suspended on
      // the continuation, not yet returned), so this whole-task cancel walk
      // visits it too.
      task.cancel()
      // Give the whole-task cancel walk a chance to run before releasing
      // `operation`.
      try? await Task.sleep(for: .milliseconds(50))
      box.continuation?.resume()
      _ = await task.value
      expectEqual(1, handlerCount.value.load(ordering: .relaxed))
    }

    // Two-clock composition: an outer deadline on ContinuousClock and an inner
    // deadline on SuspendingClock. The runtime keys records by clock identity,
    // so both records coexist independently. Sleeping past the (tight) inner
    // SuspendingClock deadline must fire the inner scope, and the outer
    // ContinuousClock deadline must remain observable via
    // `_findNearestDeadline(clock: ContinuousClock())` while inside.
    tests.test("two clock composition") {
      let cont = ContinuousClock()
      let susp = SuspendingClock()
      let contStart = cont.now
      let contDeadline = contStart.advanced(by: .seconds(600))
      let suspDeadline = susp.now.advanced(by: .milliseconds(100))

      _ = try? await withDeadline(contDeadline, clock: cont) {
        // Both records are on the task's status chain. Observing the
        // outer via its own clock must still work.
        expectEqual(contDeadline, _findNearestDeadline(clock: cont))

        do {
          try await withDeadline(suspDeadline, clock: susp) {
            try await Task.sleep(for: .seconds(30))
          }
          expectUnreachable("inner withDeadline should have thrown CancellationError")
        } catch is CancellationError {
          let elapsed = cont.now - contStart
          expectTrue(elapsed < .seconds(5))
        } catch {
          expectUnreachableCatch(error)
        }
      }
    }

    // Sanity coverage for the `withDeadline(in:)` shorthand overload. Same
    // semantics as `withDeadline(instant, clock:)` but with a duration relative
    // to `clock.now` at entry, and the clock defaulting to ContinuousClock.
    tests.test("withDeadline(in:) shorthand") {
      let clock = ContinuousClock()
      let start = clock.now
      do {
        try await withDeadline(in: .milliseconds(100)) {
          try await Task.sleep(for: .seconds(30))
        }
        expectUnreachable("withDeadline should have thrown CancellationError")
      } catch is CancellationError {
        let elapsed = clock.now - start
        expectTrue(elapsed < .seconds(5))
      } catch {
        expectUnreachableCatch(error)
      }
    }

    // The user-requested test: a `withDeadline` block whose deadline expires
    // while `operation` is running. The operation completes its own work
    // successfully (no unrelated throw), but along the way it calls
    // `Task.checkCancellation()`. That throw MUST surface a `CancellationError`
    // whose `reason` is `.deadlineExpired` (not `.unspecified`).
    tests.test("deadline checkCancellation error carries deadlineExpired reason") {
      do {
        _ = try await withDeadline(in: .milliseconds(50)) {
          // Sleep for way past the deadline. The deadline timer fires long
          // before this returns, cancels the scope, and the sleep wakes up
          // early via its withTaskCancellationHandler-installed record.
          // Swallow sleep's own thrown CancellationError with `try?` so the
          // subsequent explicit `checkCancellation()` is the one that
          // observably throws.
          try? await Task.sleep(for: .seconds(30))
          // The scope is cancelled with reason=.deadlineExpired; this
          // throw must carry that reason.
          try Task.checkCancellation()
          // Unreachable.
          return 0
        }
        expectUnreachable("withDeadline should have thrown CancellationError")
      } catch let error as CancellationError {
        // Concrete reason plumbed all the way from the timer through the
        // scope record's Reason field into Task.checkCancellation's thrown
        // CancellationError.
        expectEqual(.deadlineExpired, error.reason)
      } catch {
        expectUnreachableCatch(error)
      }
    }

    await runAllTestsAsync()
  }
}
