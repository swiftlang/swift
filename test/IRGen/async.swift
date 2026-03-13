// RUN: %target-swift-frontend -primary-file %s -enable-builtin-module -emit-ir  -target %target-swift-5.1-abi-triple | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize
// RUN: %target-swift-frontend -primary-file %s -enable-builtin-module -emit-ir  -target %target-swift-5.1-abi-triple -enable-library-evolution

// REQUIRES: concurrency
// UNSUPPORTED: CPU=wasm32

import Builtin

// CHECK: "$s5async1fyyYaF"
public func f() async { }

// CHECK: "$s5async1gyyYaKF"
public func g() async throws { }

// CHECK: "$s5async1hyyS2iYbXEF"
public func h(_: @Sendable (Int) -> Int) { }

@_silgen_name("swift_task_future_wait_throwing")
public func _taskFutureGetThrowing<T>(_ task: Builtin.NativeObject) async throws -> T

// CHECK: define{{.*}} swift{{(tail)?}}cc void @"$s5async8testThisyyBonYaF"(ptr swiftasync %0{{.*}}
// CHECK-NOT: @swift_task_alloc
// CHECK: {{(must)?}}tail call swift{{(tail)?}}cc void @swift_task_future_wait_throwing(ptr {{.*}}, ptr {{.*}}, ptr {{.*}}, ptr {{.*}}, ptr {{.*}})
public func testThis(_ task: __owned Builtin.NativeObject) async {
  do {
    let _ : Int = try await _taskFutureGetThrowing(task)
  } catch _ {
    print("error")
  }
}


public protocol P {}

struct I : P {
  var x = 0
}

public struct S {
  public func callee() async -> some P {
    return I()
  }
  // We used to assert on this in resilient mode due to mismatch function
  // pointers.
  public func caller() async -> some P {
      return await callee()
  }
}
