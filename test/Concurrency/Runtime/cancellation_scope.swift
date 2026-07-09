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
    print("done")
  }
}

@available(StdlibDeploymentTarget 6.5, *)
func test_scope_basic() async {
  print("--- test_scope_basic")
  // CHECK-LABEL: --- test_scope_basic

  await __withCancellationScope { scope in
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
    await __withCancellationScope { scope in
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

  // Cancel the scope synchronously from within `body` after installing a
  // withTaskCancellationHandler; the handler installed inside the scope
  // must fire so that Task.sleep etc. wake up. Since `CancellationScope`
  // is `~Escapable`, cancellation from a truly-external task must go
  // through indirect state (e.g. a timer job disarmed synchronously);
  // this test just verifies the local-cancel-fires-inner-handler shape.
  await __withCancellationScope { scope in
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
    await __withCancellationScope { scope in
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

  await __withCancellationScope { outer in
    await __withCancellationScope { inner in
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

// Matches the "done" line printed at the end of Main.main; must appear as
// the last CHECK in source order because FileCheck matches sequentially.
// CHECK: done
