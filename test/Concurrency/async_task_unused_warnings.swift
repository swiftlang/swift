// RUN: %target-typecheck-verify-swift
// REQUIRES: concurrency
// REQUIRES: asserts

struct MyError: Error {}

func boom() throws { throw MyError() }

@available(SwiftStdlib 6.2, *)
func test() async throws {
  Task {
    throw MyError()
  }
  // expected-warning@-3 {{unstructured throwing task created by 'init(name:priority:operation:)' is not used, which may accidentally ignore errors thrown inside the task}}
  // expected-note@-4 {{to silence this warning, handle the error inside the task, or store/discard the task value explicitly}}

  Task(executorPreference: nil) {
    throw MyError()
  }
  // expected-warning@-3 {{unstructured throwing task created by 'init(name:executorPreference:priority:operation:)' is not used, which may accidentally ignore errors thrown inside the task}}
  // expected-note@-4 {{to silence this warning, handle the error inside the task, or store/discard the task value explicitly}}

  Task.detached {
    throw MyError()
  }
  // expected-warning@-3 {{unstructured throwing task created by 'detached(name:priority:operation:)' is not used, which may accidentally ignore errors thrown inside the task}}
  // expected-note@-4 {{to silence this warning, handle the error inside the task, or store/discard the task value explicitly}}

  Task.immediate {
    throw MyError()
  }
  // expected-warning@-3 {{unstructured throwing task created by 'immediate(name:priority:executorPreference:operation:)' is not used, which may accidentally ignore errors thrown inside the task}}
  // expected-note@-4 {{to silence this warning, handle the error inside the task, or store/discard the task value explicitly}}

  Task.immediateDetached {
    throw MyError()
  }
  // expected-warning@-3 {{unstructured throwing task created by 'immediateDetached(name:priority:executorPreference:operation:)' is not used, which may accidentally ignore errors thrown inside the task}}
  // expected-note@-4 {{to silence this warning, handle the error inside the task, or store/discard the task value explicitly}}

  // Typed throws with non-Never error type — should warn
  Task { () async throws(MyError) -> Void in
    throw MyError()
  }
  // expected-warning@-3 {{unstructured throwing task created by 'init(name:priority:operation:)' is not used, which may accidentally ignore errors thrown inside the task}}
  // expected-note@-4 {{to silence this warning, handle the error inside the task, or store/discard the task value explicitly}}

  Task.detached { () async throws(MyError) -> Void in
    throw MyError()
  }
  // expected-warning@-3 {{unstructured throwing task created by 'detached(name:priority:operation:)' is not used, which may accidentally ignore errors thrown inside the task}}
  // expected-note@-4 {{to silence this warning, handle the error inside the task, or store/discard the task value explicitly}}

  // throws(Never) — should NOT warn (resolves to non-throwing overload)
  Task { () async throws(Never) -> Void in
  }

  Task.detached { () async throws(Never) -> Void in
  }

  Task.immediate { () async throws(Never) -> Void in
  }

  Task.immediateDetached { () async throws(Never) -> Void in
  }

  // Non-throwing tasks should NOT warn (discardable)
  Task {
    _ = 42
  }

  Task.detached {
    _ = 42
  }

  Task.immediate {
    _ = 42
  }

  Task.immediateDetached {
    _ = 42
  }

  // Explicitly discarded throwing tasks should NOT warn
  _ = Task {
    throw MyError()
  }

  _ = Task.detached {
    throw MyError()
  }

  _ = Task.immediate {
    throw MyError()
  }

  _ = Task.immediateDetached {
    throw MyError()
  }
}
