// RUN: %target-swift-emit-irgen                             \
// RUN:     %s                                               \
// RUN:     -enable-experimental-feature CoroutineAccessors  \
// RUN:     -enable-library-evolution                        \
// RUN: | %IRGenFileCheck %s --check-prefix=CHECK-OLD

// For now, a crash is expected when attempting to use the callee-allocated ABI: 
// it's not implemented.
// RUN: not --crash                                             \
// RUN:     %target-swift-emit-irgen                            \
// RUN:         %s                                              \
// RUN:         -enable-experimental-feature CoroutineAccessors \
// RUN:         -enable-experimental-feature CoroutineAccessorsAllocateInCallee

// REQUIRES: asserts

@frozen
public struct S {
public var o: any AnyObject
public var _i: Int = 0

public var irm: Int {
// CHECK-OLD-LABEL: define{{.*}} { ptr, {{i64|i32}} } @"$s19coroutine_accessors1SV3irmSivy"(
// CHECK-OLD-SAME:      ptr noalias dereferenceable({{32|16}}) %0,
// CHECK-OLD-SAME:      ptr %1,
// CHECK-OLD-SAME:      [[INT]] %2
// CHECK-OLD-SAME:  )
// CHECK-OLD-SAME:  {
// CHECK-OLD:       }
  read {
    yield _i
  }
// CHECK-OLD-LABEL: define{{.*}} { ptr, ptr } @"$s19coroutine_accessors1SV3irmSivx"(
// CHECK-OLD-SAME:      ptr noalias dereferenceable({{32|16}}) %0, 
// CHECK-OLD-SAME:      ptr nocapture swiftself dereferenceable({{16|8}}) %1
// CHECK-OLD-SAME:  )
// CHECK-OLD-SAME:  {
// CHECK-OLD:       }
  modify {
    yield &_i
  }
// CHECK-OLD-LABEL: define{{.*}} { ptr, {{i64|i32}} } @"$s19coroutine_accessors1SV3irmSivr"(
// CHECK-OLD-SAME:      ptr noalias dereferenceable({{32|16}}) %0,
// CHECK-OLD-SAME:      ptr %1,
// CHECK-OLD-SAME:      [[INT]] %2
// CHECK-OLD-SAME:  )
// CHECK-OLD-SAME:  {
// CHECK-OLD:       }
// CHECK-OLD-LABEL: define{{.*}} void @"$s19coroutine_accessors1SV3irmSivs"(
// CHECK-OLD-SAME:      [[INT]] %0, 
// CHECK-OLD-SAME:      ptr nocapture swiftself dereferenceable({{16|8}}) %1
// CHECK-OLD-SAME:  )
// CHECK-OLD-SAME:  {
// CHECK-OLD:       }
} // public var irm
}
