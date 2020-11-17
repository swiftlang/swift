// RUN: %target-swift-frontend -dump-ast %s -enable-experimental-concurrency | %FileCheck %s
// REQUIRES: concurrency

func acceptClosure<T>(_: () -> T) { }
func acceptEscapingClosure<T>(_: @escaping () -> T) { }

func acceptAsyncClosure<T>(_: () async -> T) { }
func acceptEscapingAsyncClosure<T>(_: @escaping () async -> T) { }

actor class MyActor {
  func method() async -> String { "" }
}

extension MyActor {
  // CHECK-LABEL: testClosureIsolation
  func testClosureIsolation() async {
    // CHECK: acceptAsyncClosure
    // CHECK: closure_expr
    // CHECK-SAME: actor-isolated=closure_isolation.(file).MyActor extension.testClosureIsolation().self
    acceptAsyncClosure { await method() }

    // CHECK: acceptAsyncClosure
    // CHECK: closure_expr
    // CHECK-NOT: actor-isolated
    acceptAsyncClosure { () async in print("hello") }

    // CHECK: acceptEscapingAsyncClosure
    // CHECK: closure_expr
    // CHECK-NOT: actor-isolated
    acceptEscapingAsyncClosure { await self.method() }

    // CHECK: acceptEscapingAsyncClosure
    // CHECK: closure_expr
    // CHECK-NOT:actor-isolated
    acceptEscapingAsyncClosure { () async in print("hello") }
  }
}

actor class SomeActor { }

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

func someAsyncFunc() async { }

// CHECK-LABEL: someGlobalActorFunc
@SomeGlobalActor func someGlobalActorFunc() async {
  // CHECK: acceptAsyncClosure
  // CHECK: closure_expr
  // CHECK-SAME: global-actor-isolated=SomeGlobalActor
  acceptAsyncClosure { await someAsyncFunc() }

  // CHECK: acceptAsyncClosure
  // CHECK: closure_expr
  // CHECK-SAME: global-actor-isolated=SomeGlobalActor
  acceptAsyncClosure { () async in print("hello") }

  // CHECK: acceptEscapingAsyncClosure
  // CHECK: closure_expr
  // CHECK-NOT: actor-isolated
  acceptEscapingAsyncClosure { await someAsyncFunc() }

  // CHECK: acceptEscapingAsyncClosure
  // CHECK: closure_expr
  // CHECK-NOT:actor-isolated
  acceptEscapingAsyncClosure { () async in print("hello") }

}
