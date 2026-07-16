// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// Parity experiments: the cancellation semantics of a `withDeadline { }` scope
// must match those of a child `Task { }` where the child is cancelled instead
// of the deadline firing. Each numbered scenario runs the SAME shape twice
// (child-task variant, then withDeadline variant) and prints identically
// tagged lines. FileCheck matches "child:" and "deadline:" outputs to prove
// they agree.
//
// Where a scenario differs from the "child task started with `Task { }`"
// analog on purpose (structured children NOT auto-cancelled via scope, etc)
// the difference is called out; but for the six cancellation observables
// covered here, `withDeadline` MUST behave identically to a spawned child
// that got externally cancelled.

@_spi(Concurrency) import _Concurrency

@available(StdlibDeploymentTarget 6.5, *)
@main struct Main {
  static func main() async {
    await scenario_1_local_cancel_observable_inside()
    await scenario_2_local_cancel_does_not_leak_to_parent()
    await scenario_3_handler_installed_inside_fires()
    await scenario_4_handler_installed_outside_does_not_fire()
    await scenario_5_shield_inside_hides_isCancelled()
    await scenario_6_ambient_task_cancel_propagates_into_scope()
    print("done")
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Helpers

@available(StdlibDeploymentTarget 6.5, *)
func runInPastDeadline<T>(
  _ operation: nonisolated(nonsending) () async throws -> T
) async -> T? {
  // A ContinuousClock deadline 1ms in the past: the deadline timer will
  // fire essentially immediately, but the `operation` body still runs at
  // least once (both `withDeadline` and a spawned child-Task-that-then-gets-cancelled
  // share this "operation always gets to observe cancellation from inside"
  // property, they don't get preempted before entry).
  let clock = ContinuousClock()
  let deadline = clock.now.advanced(by: .milliseconds(-1))
  return try? await withDeadline(deadline, clock: clock, operation: operation)
}

// ==== -----------------------------------------------------------------------
// MARK: Scenario 1
//   Local cancellation (child-task cancel / deadline expiry) is observable
//   as Task.isCancelled == true from code running inside the scope.

@available(StdlibDeploymentTarget 6.5, *)
func scenario_1_local_cancel_observable_inside() async {
  print("--- \(#function)")
  // CHECK-LABEL: --- scenario_1_local_cancel_observable_inside()

  // Child-task variant: spawn a child, cancel it, observe from inside.
  let childObserved = await Task {
    withUnsafeCurrentTask { $0?.cancel() }
    return Task.isCancelled
  }.value
  print("child: isCancelled=\(childObserved)")
  // CHECK: child: isCancelled=true

  // withDeadline variant: past deadline, observe from inside.
  //
  // The deadline timer arms asynchronously; on a machine with pathological
  // scheduling `operation` could enter before the scope is cancelled and
  // observe false. To make the observation deterministic we yield until we
  // see the cancel, capped at a generous ceiling.
  let deadlineObserved: Bool = await runInPastDeadline {
    for _ in 0..<1000 {
      if Task.isCancelled { return true }
      await Task.yield()
    }
    return Task.isCancelled
  } ?? false
  print("deadline: isCancelled=\(deadlineObserved)")
  // CHECK: deadline: isCancelled=true
}

// ==== -----------------------------------------------------------------------
// MARK: Scenario 2
//   Local cancellation (child cancel / deadline expiry) does NOT leak into
//   the enclosing task's own isCancelled state.

@available(StdlibDeploymentTarget 6.5, *)
func scenario_2_local_cancel_does_not_leak_to_parent() async {
  print("--- \(#function)")
  // CHECK-LABEL: --- scenario_2_local_cancel_does_not_leak_to_parent()

  // Child-task variant.
  await Task {
    let child = Task {
      withUnsafeCurrentTask { $0?.cancel() }
    }
    await child.value
    print("child: parent isCancelled=\(Task.isCancelled)")
    // CHECK: child: parent isCancelled=false
  }.value

  // withDeadline variant.
  await Task {
    _ = await runInPastDeadline {
      // Let the deadline actually fire so this is not a "before it armed"
      // false negative.
      for _ in 0..<1000 { if Task.isCancelled { break }; await Task.yield() }
    }
    print("deadline: parent isCancelled=\(Task.isCancelled)")
    // CHECK: deadline: parent isCancelled=false
  }.value
}

// ==== -----------------------------------------------------------------------
// MARK: Scenario 3
//   A `withTaskCancellationHandler` installed INSIDE the scope fires when
//   the scope becomes cancelled.

@available(StdlibDeploymentTarget 6.5, *)
func scenario_3_handler_installed_inside_fires() async {
  print("--- \(#function)")
  // CHECK-LABEL: --- scenario_3_handler_installed_inside_fires()

  // Child-task variant.
  let childFired: Int = await Task {
    var count = 0
    await withTaskCancellationHandler {
      withUnsafeCurrentTask { $0?.cancel() }
    } onCancel: {
      count += 1
    }
    return count
  }.value
  print("child: handler fired=\(childFired)")
  // CHECK: child: handler fired=1

  // withDeadline variant.
  let deadlineFired: Int = await runInPastDeadline { () -> Int in
    var count = 0
    await withTaskCancellationHandler {
      // Wait until the deadline actually fires so the handler installation
      // races with cancellation are not observed as "handler fired 0" false
      // negatives; scope-cancel-after-install must fire the handler once.
      for _ in 0..<1000 { if Task.isCancelled { break }; await Task.yield() }
    } onCancel: {
      count += 1
    }
    return count
  } ?? -1
  print("deadline: handler fired=\(deadlineFired)")
  // CHECK: deadline: handler fired=1
}

// ==== -----------------------------------------------------------------------
// MARK: Scenario 4
//   A `withTaskCancellationHandler` installed OUTSIDE the scope does NOT fire
//   when the scope becomes cancelled. Only whole-task cancel of the outer
//   would fire the outer handler.

@available(StdlibDeploymentTarget 6.5, *)
func scenario_4_handler_installed_outside_does_not_fire() async {
  print("--- \(#function)")
  // CHECK-LABEL: --- scenario_4_handler_installed_outside_does_not_fire()

  // Child-task variant: outer withTaskCancellationHandler in the parent,
  // child task gets its own cancel. Parent handler must NOT fire.
  await Task {
    var outerFired = 0
    await withTaskCancellationHandler {
      let child = Task {
        withUnsafeCurrentTask { $0?.cancel() }
        return Task.isCancelled
      }
      _ = await child.value
    } onCancel: {
      outerFired += 1
    }
    print("child: outer handler fired=\(outerFired)")
    // CHECK: child: outer handler fired=0
  }.value

  // withDeadline variant.
  await Task {
    var outerFired = 0
    await withTaskCancellationHandler {
      _ = await runInPastDeadline {
        for _ in 0..<1000 { if Task.isCancelled { break }; await Task.yield() }
      }
    } onCancel: {
      outerFired += 1
    }
    print("deadline: outer handler fired=\(outerFired)")
    // CHECK: deadline: outer handler fired=0
  }.value
}

// ==== -----------------------------------------------------------------------
// MARK: Scenario 5
//   `withTaskCancellationShield` inside the scope hides local cancellation
//   from Task.isCancelled, matching what a shielded read inside a spawned
//   child task sees for the child's own cancellation.

@available(StdlibDeploymentTarget 6.5, *)
func scenario_5_shield_inside_hides_isCancelled() async {
  print("--- \(#function)")
  // CHECK-LABEL: --- scenario_5_shield_inside_hides_isCancelled()

  // Child-task variant: cancel the child, then read isCancelled from inside
  // a shield. The shield masks whole-task cancellation, so read is false.
  let childShielded: Bool = await Task {
    withUnsafeCurrentTask { $0?.cancel() }
    return await withTaskCancellationShield {
      Task.isCancelled
    }
  }.value
  print("child: shielded isCancelled=\(childShielded)")
  // CHECK: child: shielded isCancelled=false

  // withDeadline variant: past deadline, read isCancelled from inside
  // a shield installed inside the operation.
  let deadlineShielded: Bool = await runInPastDeadline {
    // Wait until the scope is actually cancelled so we're not just seeing
    // "hasn't fired yet".
    for _ in 0..<1000 {
      let unshielded = Task.isCancelled
      if unshielded { break }
      await Task.yield()
    }
    return await withTaskCancellationShield {
      Task.isCancelled
    }
  } ?? true
  print("deadline: shielded isCancelled=\(deadlineShielded)")
  // CHECK: deadline: shielded isCancelled=false
}

// ==== -----------------------------------------------------------------------
// MARK: Scenario 6
//   Ambient (whole-task) cancellation of the enclosing task propagates INTO
//   a nested scope: code running inside the scope observes isCancelled=true.
//   For a spawned child task the equivalent is: parent cancels itself, child
//   observes cancel (via task-tree propagation from swift_task_cancel).

@available(StdlibDeploymentTarget 6.5, *)
func scenario_6_ambient_task_cancel_propagates_into_scope() async {
  print("--- \(#function)")
  // CHECK-LABEL: --- scenario_6_ambient_task_cancel_propagates_into_scope()

  // Child-task variant: spawn a group child, then cancel the whole group
  // (which cancels the parent's group-scope, propagating to the child).
  await Task {
    let childObserved: Bool = await withTaskGroup(of: Bool.self) { group in
      group.addTask {
        // Wait until parent's whole-task cancel reaches us.
        for _ in 0..<1000 {
          if Task.isCancelled { return true }
          await Task.yield()
        }
        return Task.isCancelled
      }
      withUnsafeCurrentTask { $0?.cancel() }
      return await group.next() ?? false
    }
    print("child: child observed parent cancel=\(childObserved)")
    // CHECK: child: child observed parent cancel=true
  }.value

  // withDeadline variant: enter a far-future withDeadline (so the deadline
  // itself never fires), then cancel the enclosing task and observe from
  // inside that Task.isCancelled is true.
  await Task {
    let clock = ContinuousClock()
    let farFuture = clock.now.advanced(by: .seconds(600))
    let observed: Bool = await withDeadline(farFuture, clock: clock) {
      withUnsafeCurrentTask { $0?.cancel() }
      return Task.isCancelled
    }
    print("deadline: scope observed parent cancel=\(observed)")
    // CHECK: deadline: scope observed parent cancel=true
  }.value
}

// Matches the "done" line printed at the end of Main.main; must appear as
// the last CHECK in source order because FileCheck matches sequentially.
// CHECK: done
