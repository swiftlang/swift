// RUN: %target-swift-frontend -emit-ir %s -target %target-swift-5.9-abi-triple | %FileCheck %s

struct G<T> {
  var t: T

  // Make sure we *don't* try to fulfill the pack from the tuple, because we cannot
  // distinguish the one-element case that way.

  // CHECK-LABEL: define {{.*}} void @"$s34variadic_generic_fulfillment_tuple1GV20fixOuterGenericParamyACyqd__qd__Qp_tGqd__qd__Qp_t_tRvd__qd__qd__Qp_tRszlF"(ptr noalias sret(%swift.opaque) %0, ptr noalias %1, {{i32|i64}} %2, ptr %"each U", ptr noalias swiftself %3)
  func fixOuterGenericParam<each U>(_ t: T) -> G<T> where T == (repeat each U) {
    return G<T>(t: t)
  }
}
