// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.3-abi-triple %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-before %s
// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.4-abi-triple %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-after %s

// REQUIRES: OS=macosx
// REQUIRES: concurrency

@available(SwiftStdlib 6.4, *)
func shielded() {
  // CHECK-DAG: call swiftcc i1 @swift_task_cancellationShieldPush()
  // CHECK-DAG: call swiftcc {} @swift_task_cancellationShieldPop()
  withTaskCancellationShield {
    print("shielded")
  }
}

// CHECK-before-DAG: declare extern_weak swiftcc i1 @swift_task_cancellationShieldPush()
// CHECK-before-DAG: declare extern_weak swiftcc {} @swift_task_cancellationShieldPop()
// CHECK-after-DAG: declare swiftcc i1 @swift_task_cancellationShieldPush()
// CHECK-after-DAG: declare swiftcc {} @swift_task_cancellationShieldPop()
