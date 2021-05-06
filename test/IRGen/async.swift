// RUN: %target-swift-frontend -primary-file %s -emit-ir -enable-experimental-concurrency | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize

// REQUIRES: concurrency

// CHECK: "$s5async1fyyYaF"
public func f() async { }

// CHECK: "$s5async1gyyYaKF"
public func g() async throws { }

// CHECK: "$s5async1hyyS2iYbXEF"
public func h(_: @Sendable (Int) -> Int) { }

public class SomeClass {}

@_silgen_name("swift_task_future_wait")
public func task_future_wait(_ task: __owned SomeClass) async throws -> Int

// CHECK: define{{.*}} swift{{(tail)?}}cc void @"$s5async8testThisyyAA9SomeClassCnYaF"(%swift.context* swiftasync %0{{.*}}
// CHECK-64: call swiftcc i8* @swift_task_alloc(i64 48)
// CHECK: {{(must)?}}tail call swift{{(tail)?}}cc void @swift_task_future_wait(
public func testThis(_ task: __owned SomeClass) async {
  do {
    let _ = try await task_future_wait(task)
  } catch _ {
    print("error")
  }
}
