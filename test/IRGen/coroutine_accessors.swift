// RUN: %target-swift-emit-irgen                                            \
// RUN:     %s                                                              \
// RUN:     -enable-experimental-feature CoroutineAccessors                 \
// RUN: | %IRGenFileCheck %s

// REQUIRES: swift_feature_CoroutineAccessors

@frozen
public struct S {
public var o: any AnyObject
public var _i: Int = 0

public var irm: Int {
// CHECK-LABEL:     define{{.*}} { ptr, {{i64|i32}} } @"$s19coroutine_accessors1SV3irmSivy"(
// CHECK-SAME:      )
// CHECK-SAME:      {
// CHECK:           }
  read {
    yield _i
  }
// CHECK-LABEL:     define{{.*}} { ptr, ptr } @"$s19coroutine_accessors1SV3irmSivx"(
// CHECK-SAME:      )
// CHECK-SAME:      {
// CHECK:           }
  modify {
    yield &_i
  }
} // public var irm
}
