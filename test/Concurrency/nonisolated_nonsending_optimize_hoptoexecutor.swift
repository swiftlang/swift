// RUN: %target-swift-frontend -module-name test -swift-version 6 -emit-sil %s | %FileCheck --implicit-check-not=hop_to_executor  %s

// REQUIRES: concurrency

// CHECK-LABEL: sil hidden [noinline] @$s4testAAyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// CHECK: } // end sil function '$s4testAAyyYaF'
@inline(never)
nonisolated(nonsending) func test() async {}

// CHECK-LABEL: sil hidden [noinline] @$s4test5test2yyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
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
// CHECK: function_ref @$s4testAAyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
// CHECK: function_ref @$s4test5test2yyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()
// CHECK: } // end sil function '$s4test6calleryyYaF'
public func caller() async {
  await test()
  await test2()
  await test3()
}
