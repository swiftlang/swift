// RUN: %target-swift-frontend -typecheck %s 2>&1 | %FileCheck %s --check-prefix=CHECK-FILE -check-prefix CHECK
// REQUIRES: concurrency
// REQUIRES: asserts

struct MyError: Error {}

@available(SwiftStdlib 6.2, *)
func test() async throws {
  Task {
    throw MyError()
  }
  // CHECK: warning: Unstructured throwing task created by 'init(name:priority:operation:)' is unused [#NoUseUnstructuredThrowingTask]
  // CHECK-FILE: [#NoUseUnstructuredThrowingTask]: <https://{{.*}}/no-use-throwing-unstructured-task>

  Task(executorPreference: nil) {
    throw MyError()
  }
  // CHECK: warning: Unstructured throwing task created by 'init(name:executorPreference:priority:operation:)' is unused [#NoUseUnstructuredThrowingTask]
  // CHECK-FILE: [#NoUseUnstructuredThrowingTask]: <https://{{.*}}/no-use-throwing-unstructured-task>

  Task.detached {
    throw MyError()
  }
  // CHECK: warning: Unstructured throwing task created by 'detached(name:priority:operation:)' is unused [#NoUseUnstructuredThrowingTask]
  // CHECK-FILE: [#NoUseUnstructuredThrowingTask]: <https://{{.*}}/no-use-throwing-unstructured-task>

  Task.immediate {
    throw MyError()
  }
  // CHECK: warning: Unstructured throwing task created by 'immediate(name:priority:executorPreference:operation:)' is unused [#NoUseUnstructuredThrowingTask]
  // CHECK-FILE: [#NoUseUnstructuredThrowingTask]: <https://{{.*}}/no-use-throwing-unstructured-task>

  Task.immediateDetached {
    throw MyError()
  }
  // CHECK: warning: Unstructured throwing task created by 'immediateDetached(name:priority:executorPreference:operation:)' is unused [#NoUseUnstructuredThrowingTask]
  // CHECK-FILE: [#NoUseUnstructuredThrowingTask]: <https://{{.*}}/no-use-throwing-unstructured-task>
}
