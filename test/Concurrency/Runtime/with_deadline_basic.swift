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

// Matches the "done" line printed at the end of Main.main; must appear as
// the last CHECK in source order because FileCheck matches sequentially.
// CHECK: done
