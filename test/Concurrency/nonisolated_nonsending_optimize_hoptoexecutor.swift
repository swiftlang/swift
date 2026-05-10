// RUN: %target-swift-frontend -module-name test -swift-version 6 -emit-sil %s | %FileCheck --implicit-check-not=hop_to_executor  %s

// REQUIRES: concurrency

// CHECK-LABEL: sil hidden [noinline] @$s4testAAyyYaF : $@convention(thin) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> () {
// CHECK: } // end sil function '$s4testAAyyYaF'
@inline(never)
nonisolated(nonsending) func test() async {}

// CHECK-LABEL: sil hidden [noinline] @$s4test5test2yyYaF : $@convention(thin) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> () {
// CHECK: } // end sil function '$s4test5test2yyYaF'
@inline(never)
nonisolated(nonsending) func test2() async {
  await test()
}

@inline(never)
func test3() async {
}

// CHECK-LABEL: sil @$s4test6calleryyYaF : $@convention(thin) @async () -> () {
// CHECK: hop_to_executor
// CHECK: function_ref @$s4testAAyyYaF : $@convention(thin) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// CHECK: function_ref @$s4test5test2yyYaF : $@convention(thin) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// CHECK: } // end sil function '$s4test6calleryyYaF'
public func caller() async {
  await test()
  await test2()
  await test3()
}

// Regression test for: https://github.com/swiftlang/swift/issues/88259

@inline(never)
nonisolated func asyncThrows() async throws {}

// CHECK-LABEL: sil hidden @$s4test0A7GH88259yyYaKF : $@convention(thin) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> @error any Error
// CHECK: try_apply {{.*}} normal [[NORMALBB:bb[0-9]+]], error [[ERRORBB:bb[0-9]+]]
// CHECK: [[NORMALBB]]
// CHECK: hop_to_executor
// CHECK: return
// CHECK: [[ERRORBB]]
// CHECK: hop_to_executor
// CHECK: throw
// CHECK: } // end sil function '$s4test0A7GH88259yyYaKF'
nonisolated(nonsending) func testGH88259() async throws {
  try await asyncThrows()
}

struct SomeError: Error {}

@inline(never)
nonisolated func asyncThrowsTyped() async throws(SomeError) {}

// CHECK-LABEL: sil hidden @$s4test0A12GH88259TypedyyYaAA9SomeErrorVYKF : $@convention(thin) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> @error SomeError
// CHECK: try_apply {{.*}} normal [[NORMALBB:bb[0-9]+]], error [[ERRORBB:bb[0-9]+]]
// CHECK: [[NORMALBB]]
// CHECK: hop_to_executor
// CHECK: return
// CHECK: [[ERRORBB]]
// CHECK: hop_to_executor
// CHECK: throw
// CHECK: } // end sil function '$s4test0A12GH88259TypedyyYaAA9SomeErrorVYKF'
nonisolated(nonsending) func testGH88259Typed() async throws(SomeError) {
  try await asyncThrowsTyped()
}

@inline(never)
nonisolated func asyncThrowsGeneric<E: Error>(_ error: E.Type = E.self) async throws(E) {}

// CHECK-LABEL: sil hidden @$s4test0A14GH88259GenericyyxYaxYKs5ErrorRzlF : $@convention(thin) @caller_isolated @async <E where E : Error> (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @in_guaranteed E) -> @error_indirect E
// CHECK: try_apply {{.*}} normal [[NORMALBB:bb[0-9]+]], error [[ERRORBB:bb[0-9]+]]
// CHECK: [[NORMALBB]]
// CHECK: hop_to_executor
// CHECK: return
// CHECK: [[ERRORBB]]
// CHECK: hop_to_executor
// CHECK: throw_addr
// CHECK: } // end sil function '$s4test0A14GH88259GenericyyxYaxYKs5ErrorRzlF'
nonisolated(nonsending) func testGH88259Generic<E: Error>(_ error: E) async throws(E) {
  try await asyncThrowsGeneric(E.self)
}
