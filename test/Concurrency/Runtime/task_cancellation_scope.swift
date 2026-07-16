// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@_spi(Concurrency) import _Concurrency
import Dispatch

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
    await test_scope_structured_children_not_auto_cancelled()
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
func test_scope_structured_children_not_auto_cancelled() async {
  print("--- test_scope_structured_children_not_auto_cancelled")
  // CHECK: --- test_scope_structured_children_not_auto_cancelled

  // Documented behavior: scope.cancel() does NOT propagate cancellation
  // into structured children (TaskGroup / async let); those observe the
  // scope only through the same local Task.isCancelled check as any other
  // code and they inherit the parent task's own cancellation state, which
  // is untouched by scope.cancel().
  await __withTaskCancellationScope { scope in
    await withTaskGroup(of: Bool.self) { group in
      scope.cancel()

      group.addTask {
        // Child was spawned AFTER scope.cancel(); the child task itself
        // has never had its own cancellation flag set, so it must observe
        // as not-cancelled.
        Task.isCancelled
      }

      let childCancelled = await group.next() ?? true
      print("child isCancelled=\(childCancelled)")
      // CHECK: child isCancelled=false
    }
  }
}

// Matches the "done" line printed at the end of Main.main; must appear as
// the last CHECK in source order because FileCheck matches sequentially.
// CHECK: done
