// RUN: %target-swift-frontend -module-name test -swift-version 6 -emit-sil %s | %FileCheck --implicit-check-not=hop_to_executor  %s

// REQUIRES: concurrency

// CHECK-LABEL: sil hidden [noinline] @$s4testAAyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> () {
// CHECK: } // end sil function '$s4testAAyyYaF'
@inline(never)
nonisolated(nonsending) func test() async {}

// CHECK-LABEL: sil hidden [noinline] @$s4test5test2yyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> () {
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
// CHECK: function_ref @$s4testAAyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// CHECK: function_ref @$s4test5test2yyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> ()
// CHECK: } // end sil function '$s4test6calleryyYaF'
public func caller() async {
  await test()
  await test2()
  await test3()
}

// TODO: make these better...
actor A {
  // CHECK-LABEL: sil hidden [noinline] @$s4test1AC11ninsClosureyyyyYaYCXEYaF : $@convention(method) @async (@guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> (), @sil_isolated @guaranteed A) -> () {
  // CHECK: hop_to_executor
  // CHECK: {{.*}} = apply %0({{.*}})
  // CHECK: } // end sil function '$s4test1AC11ninsClosureyyyyYaYCXEYaF'
  @inline(never)
  func ninsClosure(
    _ doit: nonisolated(nonsending) () async -> Void
  ) async {
    await doit()
  }
}

// CHECK-LABEL: sil hidden @$s4test3bugyyYaF : $@convention(thin) @async () -> () {
// CHECK: hop_to_executor
// CHECK: {{.*}} = function_ref {{.*}}ninsClosure{{.*}}
// CHECK: {{.*}} = apply
// CHECK: hop_to_executor
// CHECK: } // end sil function '$s4test3bugyyYaF'
nonisolated func bug() async {
  let a = A()
  await a.ninsClosure {
    a.assertIsolated()
  }
}
