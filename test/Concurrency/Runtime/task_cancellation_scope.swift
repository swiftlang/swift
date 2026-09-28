// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@_spi(Concurrency) import _Concurrency
import Dispatch
import Synchronization

@available(StdlibDeploymentTarget 6.5, *)
@main struct Main {
  static func main() async {
    await test_scope_basic()
    await test_scope_ambient_task_uncancelled()
    await test_scope_handler_fires()
    await test_scope_handler_fires_once_on_double_cancel()
    await test_scope_nested()
    await test_scope_returns_value()
    await test_scope_typed_throws_propagates()
    await test_scope_cancel_is_idempotent()
    await test_scope_sequential_scopes_independent()
    await test_scope_ambient_cancel_before_entry_visible()
    await test_scope_ambient_cancel_while_inside_visible()
    await test_scope_handler_outside_does_not_fire_on_scope_cancel()
    await test_scope_with_cancellation_shield_inside()
    await test_scope_with_cancellation_shield_outside()
    await test_scope_outer_cancel_cascades_to_inner()
    await test_scope_structured_children_are_cascaded()
    await test_scope_async_let_child_is_cancelled()
    await test_task_handle_does_not_observe_scope_cancellation()
    await test_task_cancel_cancels_scopes()
    await test_scope_created_inside_cancelled_task()
    await test_task_cancel_does_not_cancel_scope_inside_shield()
    await test_scope_cancel_does_not_fire_handler_inside_shield()
    await test_scope_cancel_does_not_cancel_children_inside_shield()
    await test_scope_cancel_does_not_cancel_scope_inside_shield()
    await test_scope_cancelled_from_two_threads()
    print("done")
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_basic() async {
  print("--- test_scope_basic")
  // CHECK-LABEL: --- test_scope_basic

  await __withTaskCancellationScope { scope in
    print("before cancel: isCancelled=\(Task.isCancelled)")
    // CHECK: before cancel: isCancelled=false

    scope.cancel()

    print("after cancel: isCancelled=\(Task.isCancelled)")
    // CHECK: after cancel: isCancelled=true
  }

  // Outside the scope, the ambient task is unaffected.
  print("outside scope: isCancelled=\(Task.isCancelled)")
  // CHECK: outside scope: isCancelled=false
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_ambient_task_uncancelled() async {
  print("--- test_scope_ambient_task_uncancelled")
  // CHECK: --- test_scope_ambient_task_uncancelled

  await Task {
    await __withTaskCancellationScope { scope in
      scope.cancel()
      print("inside scope: isCancelled=\(Task.isCancelled)")
      // CHECK: inside scope: isCancelled=true
    }

    // The Task's own cancellation flag was never set by scope.cancel().
    print("task after scope: isCancelled=\(Task.isCancelled)")
    // CHECK: task after scope: isCancelled=false
  }.value
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_handler_fires() async {
  print("--- test_scope_handler_fires")
  // CHECK: --- test_scope_handler_fires

  // Cancel the scope synchronously from within `operation` after installing a
  // withTaskCancellationHandler; the handler installed inside the scope
  // must fire so that Task.sleep etc. wake up. Since `TaskCancellationScope`
  // is `~Escapable`, cancellation from a truly-external task must go
  // through indirect state (e.g. a timer job disarmed synchronously);
  // this test just verifies the local-cancel-fires-inner-handler shape.
  await __withTaskCancellationScope { scope in
    var fireCount = 0
    await withTaskCancellationHandler {
      scope.cancel()
    } onCancel: {
      fireCount += 1
    }

    print("fireCount=\(fireCount)")
    // CHECK: fireCount=1
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_handler_fires_once_on_double_cancel() async {
  print("--- test_scope_handler_fires_once_on_double_cancel")
  // CHECK: --- test_scope_handler_fires_once_on_double_cancel

  // A handler installed inside a scope must fire at most once, even when
  // both a scope.cancel() AND a subsequent whole-task cancel target it.
  await Task {
    await __withTaskCancellationScope { scope in
      var fireCount = 0
      await withTaskCancellationHandler {
        // Fire path #1: scope cancellation.
        scope.cancel()
        // Fire path #2: whole-task cancellation (walks the same
        // CancellationNotificationStatusRecord). Handlers are fired-once
        // by construction, so this second event must NOT re-invoke onCancel.
        withUnsafeCurrentTask { $0?.cancel() }
      } onCancel: {
        fireCount += 1
      }

      print("fireCount=\(fireCount)")
      // CHECK: fireCount=1
    }
  }.value
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_nested() async {
  print("--- test_scope_nested")
  // CHECK: --- test_scope_nested

  await __withTaskCancellationScope { outer in
    await __withTaskCancellationScope { inner in
      inner.cancel()
      print("inner cancelled, isCancelled=\(Task.isCancelled)")
      // CHECK: inner cancelled, isCancelled=true
    }

    // Only the inner scope was cancelled; outer is still live.
    print("after inner exit, isCancelled=\(Task.isCancelled)")
    // CHECK: after inner exit, isCancelled=false

    outer.cancel()
    print("outer cancelled, isCancelled=\(Task.isCancelled)")
    // CHECK: outer cancelled, isCancelled=true
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_returns_value() async {
  print("--- test_scope_returns_value")
  // CHECK: --- test_scope_returns_value

  let result = await __withTaskCancellationScope { _ in
    42
  }
  print("result=\(result)")
  // CHECK: result=42
}

@available(StdlibDeploymentTarget 6.5, *)
struct ScopeErr: Error, CustomStringConvertible {
  let tag: String
  var description: String { "ScopeErr(\(tag))" }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_typed_throws_propagates() async {
  print("--- test_scope_typed_throws_propagates")
  // CHECK: --- test_scope_typed_throws_propagates

  do {
    _ = try await __withTaskCancellationScope { _ throws(ScopeErr) -> Int in
      throw ScopeErr(tag: "boom")
    }
    print("unreachable")
  } catch {
    // The typed-throws error must flow through unchanged.
    print("caught \(error)")
    // CHECK: caught ScopeErr(boom)
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_cancel_is_idempotent() async {
  print("--- test_scope_cancel_is_idempotent")
  // CHECK: --- test_scope_cancel_is_idempotent

  await __withTaskCancellationScope { scope in
    scope.cancel()
    scope.cancel()
    scope.cancel()
    print("isCancelled=\(Task.isCancelled)")
    // CHECK: isCancelled=true
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_sequential_scopes_independent() async {
  print("--- test_scope_sequential_scopes_independent")
  // CHECK: --- test_scope_sequential_scopes_independent

  // Cancelling one scope must not leave any residue on the task's own
  // isCancelled state that a later scope would then observe.
  await __withTaskCancellationScope { first in
    first.cancel()
    print("first: \(Task.isCancelled)")
    // CHECK: first: true
  }

  print("between: \(Task.isCancelled)")
  // CHECK: between: false

  await __withTaskCancellationScope { _ in
    // Never call cancel() on the second scope.
    print("second: \(Task.isCancelled)")
    // CHECK: second: false
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_ambient_cancel_before_entry_visible() async {
  print("--- test_scope_ambient_cancel_before_entry_visible")
  // CHECK: --- test_scope_ambient_cancel_before_entry_visible

  // A pre-cancelled task must still observe as cancelled inside the scope:
  // whole-task cancellation and scope cancellation OR together on the
  // isCancelled fast path.
  await Task {
    withUnsafeCurrentTask { $0?.cancel() }
    await __withTaskCancellationScope { _ in
      print("inside pre-cancelled task: \(Task.isCancelled)")
      // CHECK: inside pre-cancelled task: true
    }
  }.value
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_ambient_cancel_while_inside_visible() async {
  print("--- test_scope_ambient_cancel_while_inside_visible")
  // CHECK: --- test_scope_ambient_cancel_while_inside_visible

  // Cancelling the whole task from within the scope's operation must be
  // observed via Task.isCancelled inside the same scope.
  await Task {
    await __withTaskCancellationScope { _ in
      print("before ambient cancel: \(Task.isCancelled)")
      // CHECK: before ambient cancel: false
      withUnsafeCurrentTask { $0?.cancel() }
      print("after ambient cancel: \(Task.isCancelled)")
      // CHECK: after ambient cancel: true
    }
  }.value
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_handler_outside_does_not_fire_on_scope_cancel() async {
  print("--- test_scope_handler_outside_does_not_fire_on_scope_cancel")
  // CHECK: --- test_scope_handler_outside_does_not_fire_on_scope_cancel

  // A handler installed OUTSIDE the scope must not fire when the scope is
  // cancelled: scope cancellation only walks records inside the scope's
  // dynamic extent.
  await Task {
    var outerHandlerCount = 0
    await withTaskCancellationHandler {
      await __withTaskCancellationScope { scope in
        scope.cancel()
        print("inside scope: \(Task.isCancelled)")
        // CHECK: inside scope: true
      }
    } onCancel: {
      outerHandlerCount += 1
    }
    // Reaching this line means the outer handler did not fire during
    // scope.cancel(); the task's own cancellation flag was never set.
    print("outer handler fired: \(outerHandlerCount)")
    // CHECK: outer handler fired: 0
  }.value
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_with_cancellation_shield_inside() async {
  print("--- test_scope_with_cancellation_shield_inside")
  // CHECK: --- test_scope_with_cancellation_shield_inside

  await Task {
    await __withTaskCancellationScope { scope in
      scope.cancel()
      print("inside scope: \(Task.isCancelled)")
      // CHECK: inside scope: true
      await withTaskCancellationShield {
        print("inside scope, and shield: \(Task.isCancelled)")
        // CHECK: inside scope, and shield: false
      }
    }
  }.value
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_with_cancellation_shield_outside() async {
  print("--- test_scope_with_cancellation_shield_outside")
  // CHECK: --- test_scope_with_cancellation_shield_outside

  await Task {
    await withTaskCancellationShield {
      await __withTaskCancellationScope { scope in
        scope.cancel()
        // The outer shield is OUTSIDE the scope on the record chain, so
        // walking innermost-first the walker hits the cancelled scope
        // BEFORE the shield - the shield does not mask it. Same as a
        // child task cancelled from within: a shield in the parent
        // doesn't hide the child's own cancellation.
        print("inside scope, and outer shield: \(Task.isCancelled)")
        // CHECK: inside scope, and outer shield: true
        await withTaskCancellationShield {
          // This inner shield IS inside the scope. Walking innermost-first
          // the walker now hits this shield first and short-circuits to
          // nullptr - masks the cancellation.
          print("inside scope, and inner shield: \(Task.isCancelled)")
          // CHECK: inside scope, and inner shield: false
        }
      }
    }
  }.value
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_outer_cancel_cascades_to_inner() async {
  print("--- test_scope_outer_cancel_cascades_to_inner")
  // CHECK: --- test_scope_outer_cancel_cascades_to_inner

  // Cancelling an outer scope must also cancel any nested inner scopes,
  // matching "as-if child task" semantics: cancelling a parent cancels
  // its children. Uses the SPI `scope.isCancelled` to read each scope's
  // own flag directly, independent of Task.isCancelled.
  await __withTaskCancellationScope { outer in
    await __withTaskCancellationScope { inner in
      print("before: outer=\(outer.isCancelled) inner=\(inner.isCancelled)")
      // CHECK: before: outer=false inner=false

      outer.cancel()

      print("after outer.cancel: outer=\(outer.isCancelled) inner=\(inner.isCancelled)")
      // CHECK: after outer.cancel: outer=true inner=true

      // Task.isCancelled also observes the cascade: the walker hits the
      // innermost scope (now cancelled) first and returns true.
      print("Task.isCancelled=\(Task.isCancelled)")
      // CHECK: Task.isCancelled=true
    }
  }
}

@available(StdlibDeploymentTarget 6.5, *)
struct Signal: Sendable {
  private let stream: AsyncStream<Void>
  private let continuation: AsyncStream<Void>.Continuation

  init() {
    (stream, continuation) = AsyncStream<Void>.makeStream()
  }

  func signal() {
    continuation.yield(())
  }

  func wait() async {
    var iterator = stream.makeAsyncIterator()
    _ = await iterator.next()
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_structured_children_are_cascaded() async {
  print("--- test_scope_structured_children_are_cascaded")
  // CHECK: --- test_scope_structured_children_are_cascaded

  // scope.cancel() cascades into structured children: children already
  // spawned when cancel runs get cancelled via the record-chain walk;
  // children spawned AFTER cancel are cancelled at creation because their
  // parent has a cancelled scope on its chain.
  await __withTaskCancellationScope { scope in
    await withTaskGroup(of: Bool.self) { group in
      let childStarted = Signal()
      let scopeCancelled = Signal()

      // Child spawned BEFORE cancel: still executing when the cascade
      // fires; observes as cancelled.
      group.addTask {
        childStarted.signal()
        await scopeCancelled.wait()
        return Task.isCancelled
      }

      await childStarted.wait()

      scope.cancel()
      scopeCancelled.signal()

      group.addTask {
        Task.isCancelled
      }

      let a = await group.next() ?? false
      let b = await group.next() ?? false
      print("child (before cancel) isCancelled=\(a)")
      // CHECK: child (before cancel) isCancelled=true
      print("child (after cancel) isCancelled=\(b)")
      // CHECK: child (after cancel) isCancelled=true
    }
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_async_let_child_is_cancelled() async {
  print("--- test_scope_async_let_child_is_cancelled")
  // CHECK: --- test_scope_async_let_child_is_cancelled

  // `async let` children can never escape the scope that spawned them - the
  // enclosing `await` at the end of the scope's `operation` guarantees they
  // complete before the scope does. So cancelling the scope must cancel any
  // `async let` child too: ones already running (cascaded via the
  // record-chain walk in swift_task_cancelCancellationScopeImpl)
  // and ones spawned after cancel (cancelled at creation, since their parent
  // already has a cancelled scope on its chain).
  await __withTaskCancellationScope { scope in
    let childStarted = Signal()
    let scopeCancelled = Signal()

    async let before: Bool = {
      childStarted.signal()
      await scopeCancelled.wait()
      return Task.isCancelled
    }()

    await childStarted.wait()

    scope.cancel()
    scopeCancelled.signal()

    async let after: Bool = Task.isCancelled

    let a = await before
    let b = await after
    print("async let (before cancel) isCancelled=\(a)")
    // CHECK: async let (before cancel) isCancelled=true
    print("async let (after cancel) isCancelled=\(b)")
    // CHECK: async let (after cancel) isCancelled=true
  }
}

// A signal whose `wait()` ignores cancellation by spinning.
@available(StdlibDeploymentTarget 6.5, *)
final class CancellationIgnoringSignal: Sendable {
  private let signalled = Atomic<Bool>(false)

  func signal() {
    signalled.store(true, ordering: .releasing)
  }

  func wait() async {
    while !signalled.load(ordering: .acquiring) {
      await Task.yield()
    }
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_task_handle_does_not_observe_scope_cancellation() async {
  print("--- test_task_handle_does_not_observe_scope_cancellation")
  // CHECK: --- test_task_handle_does_not_observe_scope_cancellation

  // `isCancelled` on a task handle reports the cancellation of the task itself.
  // The cancellation of a scope is only visible to the code inside the scope.
  let entered = Signal()
  let checked = CancellationIgnoringSignal()

  let task = Task {
    await __withTaskCancellationScope { scope in
      scope.cancel(reason: .deadlineExpired)
      print("inside scope: Task.isCancelled=\(Task.isCancelled)")
      // CHECK: inside scope: Task.isCancelled=true
      print("inside scope: Task.cancellationReason=\(Task.cancellationReason.map { "\($0)" } ?? "nil")")
      // CHECK: inside scope: Task.cancellationReason=deadlineExpired
      let unsafeIsCancelled = withUnsafeCurrentTask { $0!.isCancelled }
      print("inside scope: UnsafeCurrentTask.isCancelled=\(unsafeIsCancelled)")
      // CHECK: inside scope: UnsafeCurrentTask.isCancelled=false
      let unsafeReason = withUnsafeCurrentTask { $0!.cancellationReason }
      print("inside scope: UnsafeCurrentTask.cancellationReason=\(unsafeReason.map { "\($0)" } ?? "nil")")
      // CHECK: inside scope: UnsafeCurrentTask.cancellationReason=nil

      entered.signal()
      await checked.wait()
    }
  }

  await entered.wait()
  print("handle while inside scope: isCancelled=\(task.isCancelled)")
  // CHECK: handle while inside scope: isCancelled=false

  // The cancellation of the task itself is visible through the handle.
  task.cancel()
  print("handle after cancel: isCancelled=\(task.isCancelled)")
  // CHECK: handle after cancel: isCancelled=true

  checked.signal()
  await task.value
}

@available(StdlibDeploymentTarget 6.5, *)
func test_task_cancel_cancels_scopes() async {
  print("--- test_task_cancel_cancels_scopes")
  // CHECK: --- test_task_cancel_cancels_scopes

  // Cancelling a task also cancels all of its scopes, the same as cancelling a
  // task cancels its child tasks.
  await Task {
    await __withTaskCancellationScope { outer in
      await __withTaskCancellationScope { inner in
        withUnsafeCurrentTask { $0?.cancel() }
        print("outer scope isCancelled=\(outer.isCancelled)")
        // CHECK: outer scope isCancelled=true
        print("inner scope isCancelled=\(inner.isCancelled)")
        // CHECK: inner scope isCancelled=true
      }
    }
  }.value
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_created_inside_cancelled_task() async {
  print("--- test_scope_created_inside_cancelled_task")
  // CHECK: --- test_scope_created_inside_cancelled_task

  // A scope created inside a cancelled task starts out cancelled.
  await Task {
    withUnsafeCurrentTask { $0?.cancel() }
    await __withTaskCancellationScope { outer in
      print("scope isCancelled=\(outer.isCancelled)")
      // CHECK: scope isCancelled=true
      await __withTaskCancellationScope { inner in
        print("nested scope isCancelled=\(inner.isCancelled)")
        // CHECK: nested scope isCancelled=true
      }
    }
  }.value
}

@available(StdlibDeploymentTarget 6.5, *)
func test_task_cancel_does_not_cancel_scope_inside_shield() async {
  print("--- test_task_cancel_does_not_cancel_scope_inside_shield")
  // CHECK: --- test_task_cancel_does_not_cancel_scope_inside_shield

  // A shield prevents the cancellation of the task from scopes inside of it, no
  // matter if they are created before or after the task got cancelled.
  await Task {
    await withTaskCancellationShield {
      await __withTaskCancellationScope { scope in
        withUnsafeCurrentTask { $0?.cancel() }
        print("scope in shield isCancelled=\(scope.isCancelled)")
        // CHECK: scope in shield isCancelled=false
      }
      await __withTaskCancellationScope { scope in
        print("scope created in shield isCancelled=\(scope.isCancelled)")
        // CHECK: scope created in shield isCancelled=false
      }
    }
  }.value
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_cancel_does_not_fire_handler_inside_shield() async {
  print("--- test_scope_cancel_does_not_fire_handler_inside_shield")
  // CHECK: --- test_scope_cancel_does_not_fire_handler_inside_shield

  // A shield inside a scope prevents the cancellation of the shield's scope, so a
  // handler installed inside the shield must not fire. A handler installed
  // inside the scope but outside the shield still fires.
  await __withTaskCancellationScope { scope in
    var outsideFired = false
    var insideFired = false
    await withTaskCancellationHandler {
      await withTaskCancellationShield {
        await withTaskCancellationHandler {
          scope.cancel()
          print("in shield: isCancelled=\(Task.isCancelled)")
          // CHECK: in shield: isCancelled=false
        } onCancel: {
          insideFired = true
        }
      }
    } onCancel: {
      outsideFired = true
    }
    print("handler outside shield fired=\(outsideFired)")
    // CHECK: handler outside shield fired=true
    print("handler inside shield fired=\(insideFired)")
    // CHECK: handler inside shield fired=false
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_cancel_does_not_cancel_children_inside_shield() async {
  print("--- test_scope_cancel_does_not_cancel_children_inside_shield")
  // CHECK: --- test_scope_cancel_does_not_cancel_children_inside_shield

  // Child tasks created inside a shield are not cancelled by the cancellation
  // of the scope, no matter if they are created before or after the scope got
  // cancelled.
  await __withTaskCancellationScope { scope in
    await withTaskCancellationShield {
      let childStarted = Signal()
      let scopeCancelled = Signal()

      async let asyncLetChild: Bool = {
        childStarted.signal()
        await scopeCancelled.wait()
        return Task.isCancelled
      }()

      await withTaskGroup(of: Bool.self) { group in
        let groupChildStarted = Signal()
        group.addTask {
          groupChildStarted.signal()
          await scopeCancelled.wait()
          return Task.isCancelled
        }

        await childStarted.wait()
        await groupChildStarted.wait()
        scope.cancel()
        scopeCancelled.signal()
        scopeCancelled.signal()

        print("group child in shield isCancelled=\(await group.next()!)")
        // CHECK: group child in shield isCancelled=false
      }
      print("async let in shield isCancelled=\(await asyncLetChild)")
      // CHECK: async let in shield isCancelled=false
    }
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_cancel_does_not_cancel_scope_inside_shield() async {
  print("--- test_scope_cancel_does_not_cancel_scope_inside_shield")
  // CHECK: --- test_scope_cancel_does_not_cancel_scope_inside_shield

  // A scope inside a shield is not cancelled by the cancellation of an outer
  // scope, the same as a scope that is created inside a shield after the outer
  // scope got cancelled.
  await __withTaskCancellationScope { outer in
    await withTaskCancellationShield {
      await __withTaskCancellationScope { inner in
        outer.cancel()
        print("inner scope isCancelled=\(inner.isCancelled)")
        // CHECK: inner scope isCancelled=false
        print("Task.isCancelled=\(Task.isCancelled)")
        // CHECK: Task.isCancelled=false
      }
    }
  }
}

// TODO: The below method and silgen methods are needed right now since
// TaskCancellationScope is ~Copyable and ~Sendable. Once we do that
// we can remove those.
@available(StdlibDeploymentTarget 6.5, *)
func withScopeRecord(_ body: (ScopeRecord) async -> Void) async {
  let scope = ScopeRecord(pointer: _pushCancellationScope())
  await body(scope)
  _popCancellationScope(scope.pointer)
}

struct ScopeRecord: @unchecked Sendable {
  let pointer: UnsafeRawPointer
}

@_silgen_name("swift_task_pushCancellationScope")
func _pushCancellationScope() -> UnsafeRawPointer

@_silgen_name("swift_task_popCancellationScope")
func _popCancellationScope(_ record: UnsafeRawPointer)

@_silgen_name("swift_task_cancelCancellationScope")
func _cancelCancellationScope(_ record: UnsafeRawPointer, _ flags: UInt)

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_cancelled_from_two_threads() async {
  print("--- test_scope_cancelled_from_two_threads")
  // CHECK: --- test_scope_cancelled_from_two_threads

  // A scope is only cancelled once. If two threads cancel it at the same time,
  // the handlers inside of the scope get the reason of the scope. This is quite
  // hard to setup so the test isn't simple.
  let continuation = Mutex<CheckedContinuation<Void, Never>?>(nil)
  let task = Task(priority: .low) {
    await withScopeRecord { scope in
      let otherThread = DispatchGroup()
      let handlerReason = Mutex<CancellationError.Reason?>(nil)
      await withTaskCancellationHandler {
        await withTaskPriorityEscalationHandler {
          await withCheckedContinuation { c in
            continuation.withLock { $0 = c }
          }
        } onPriorityEscalated: { _, _ in
          // This runs while the escalating thread holds the status record
          // lock of the task. Another thread cancels the scope while we wait,
          // and then we cancel it with a different reason.
          otherThread.enter()
          DispatchQueue.global().async {
            _cancelCancellationScope(scope.pointer, 0) // .unspecified
            otherThread.leave()
          }
          _ = DispatchSemaphore(value: 0).wait(timeout: .now() + .milliseconds(100))
          _cancelCancellationScope(scope.pointer, 1) // .deadlineExpired
        }
      } onCancel: { reason in
        handlerReason.withLock { $0 = reason }
      }
      otherThread.wait()
      let reason = handlerReason.withLock { $0 }
      print("handler reason is scope reason: \(reason == Task.cancellationReason)")
      // CHECK: handler reason is scope reason: true
    }
  }

  while continuation.withLock({ $0 == nil }) {
    await Task.yield()
  }
  task.escalatePriority(to: .high)
  continuation.withLock { $0!.resume() }
  await task.value
}

// CHECK: done
