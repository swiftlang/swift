// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5 -enable-experimental-concurrency -enable-experimental-async-handler | %FileCheck %s
// REQUIRES: concurrency

func take<T>(_ t: T) async {
  print(t)
}

// CHECK-LABEL: sil [ossa] @$s4test13simpleHandleryySiF : $@convention(thin) (Int) -> () {
// CHECK:   [[BODYFN:%[0-9]+]] = function_ref @$s4test13simpleHandleryySiYaF : $@convention(thin) @async (Int) -> ()
// CHECK:   [[FN:%[0-9]+]] = partial_apply [callee_guaranteed] [[BODYFN]](%0) : $@convention(thin) @async (Int) -> ()
// CHECK:   [[INTRINSIC:%[0-9]+]] = function_ref @$ss16_runAsyncHandler9operationyyyYac_tF : $@convention(thin) (@guaranteed @async @callee_guaranteed () -> ()) -> ()
// CHECK:   {{.*}} = apply [[INTRINSIC]]([[FN]]) : $@convention(thin) (@guaranteed @async @callee_guaranteed () -> ()) -> ()
// CHECK:   destroy_value [[FN]] : $@async @callee_guaranteed () -> ()
// CHECK: } // end sil function '$s4test13simpleHandleryySiF'
@asyncHandler
public func simpleHandler(_ i: Int) {
  await take(i)
}

// CHECK-LABEL: sil [ossa] @$s4test20nonTrivialArgHandleryySSF : $@convention(thin) (@guaranteed String) -> () {
// CHECK:   [[COPY:%[0-9]+]] = copy_value %0 : $String
// CHECK:   [[BODYFN:%[0-9]+]] = function_ref @$s4test20nonTrivialArgHandleryySSYaF : $@convention(thin) @async (@guaranteed String) -> ()
// CHECK:   [[FN:%[0-9]+]] = partial_apply [callee_guaranteed] [[BODYFN]]([[COPY]]) : $@convention(thin) @async (@guaranteed String) -> ()
// CHECK:   [[INTRINSIC:%[0-9]+]] = function_ref @$ss16_runAsyncHandler9operationyyyYac_tF : $@convention(thin) (@guaranteed @async @callee_guaranteed () -> ()) -> ()
// CHECK:   {{.*}} = apply [[INTRINSIC]]([[FN]]) : $@convention(thin) (@guaranteed @async @callee_guaranteed () -> ()) -> ()
// CHECK:   destroy_value [[FN]] : $@async @callee_guaranteed () -> ()
// CHECK: } // end sil function '$s4test20nonTrivialArgHandleryySSF'
@asyncHandler
public func nonTrivialArgHandler(_ s: String) {
  await take(s)
}

// CHECK-LABEL: sil [ossa] @$s4test14genericHandleryyxlF : $@convention(thin) <T> (@in_guaranteed T) -> () {
// CHECK:   [[TMP:%[0-9]+]] = alloc_stack $T
// CHECK:   copy_addr %0 to [initialization] [[TMP]] : $*T
// CHECK:   [[BODYFN:%[0-9]+]] = function_ref @$s4test14genericHandleryyxYalF : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[FN:%[0-9]+]] = partial_apply [callee_guaranteed] [[BODYFN]]<T>([[TMP]]) : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[INTRINSIC:%[0-9]+]] = function_ref @$ss16_runAsyncHandler9operationyyyYac_tF : $@convention(thin) (@guaranteed @async @callee_guaranteed () -> ()) -> ()
// CHECK:   {{.*}} = apply [[INTRINSIC]]([[FN]]) : $@convention(thin) (@guaranteed @async @callee_guaranteed () -> ()) -> ()
// CHECK:   destroy_value [[FN]] : $@async @callee_guaranteed () -> ()
// CHECK: } // end sil function '$s4test14genericHandleryyxlF'
@asyncHandler
public func genericHandler<T>(_ t: T) {
  await take(t)
}

public struct Mystruct {
  // CHECK-LABEL: sil [ossa] @$s4test8MystructV13memberHandleryySiF : $@convention(method) (Int, Mystruct) -> () {
  // CHECK:   [[BODYFN:%[0-9]+]] = function_ref @$s4test8MystructV13memberHandleryySiYaF : $@convention(method) @async (Int, Mystruct) -> ()
  // CHECK:   [[FN:%[0-9]+]] = partial_apply [callee_guaranteed] [[BODYFN]](%0, %1) : $@convention(method) @async (Int, Mystruct) -> ()
  // CHECK:   [[INTRINSIC:%[0-9]+]] = function_ref @$ss16_runAsyncHandler9operationyyyYac_tF : $@convention(thin) (@guaranteed @async @callee_guaranteed () -> ()) -> ()
  // CHECK:   {{.*}} = apply [[INTRINSIC]]([[FN]]) : $@convention(thin) (@guaranteed @async @callee_guaranteed () -> ()) -> ()
  // CHECK:   destroy_value [[FN]] : $@async @callee_guaranteed () -> ()
  // CHECK: } // end sil function '$s4test8MystructV13memberHandleryySiF'
  @asyncHandler
  public func memberHandler(_ i: Int) {
    await take(i)
  }
}
