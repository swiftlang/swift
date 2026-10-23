// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.3-abi-triple %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-before %s
// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.4-abi-triple %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-after %s

// REQUIRES: OS=macosx
// REQUIRES: concurrency

@available(SwiftStdlib 6.4, *)
func shielded() {
  // CHECK: call swiftcc i1 @swift_task_cancellationShieldPush()
  // CHECK: call swiftcc {} @swift_task_cancellationShieldPop()
  withTaskCancellationShield {
    print("shielded")
  }
}

// CHECK-before: declare extern_weak swiftcc i1 @swift_task_cancellationShieldPush()
// CHECK-before: declare extern_weak swiftcc {} @swift_task_cancellationShieldPop()
// CHECK-after: declare swiftcc i1 @swift_task_cancellationShieldPush()
// CHECK-after: declare swiftcc {} @swift_task_cancellationShieldPop()
