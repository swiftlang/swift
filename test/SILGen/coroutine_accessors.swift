// RUN: %target-swift-emit-silgen \
// RUN:     %s \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN: | %FileCheck %s

public struct S {
public var o: any AnyObject
public var _i: Int = 0

public var irm: Int {
  _read {
    yield _i
  }
// CHECK-LABEL: sil [ossa] @$s19coroutine_accessors1SV3irmSivx : 
// CHECK-SAME:      $@yield_once
// CHECK-SAME:      @convention(method)
// CHECK-SAME:      (@inout S)
// CHECK-SAME:      ->
// CHECK-SAME:      @yields @inout Int
// CHECK-SAME:  {
// CHECK-LABEL: } // end sil function '$s19coroutine_accessors1SV3irmSivx'
  modify {
    yield &_i
  }
// CHECK-LABEL: sil {{.*}}[ossa] @$s19coroutine_accessors1SV3irmSivs :
// CHECK-SAME:      $@convention(method)
// CHECK-SAME:      (Int, @inout S)
// CHECK-SAME:      ->
// CHECK-SAME:      ()
// CHECK-SAME:  {
// CHECK:       bb0(
// CHECK-SAME:      [[NEW_VALUE:%[^,]+]] :
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME: ):
// CHECK:        [[SELF_ACCESS:%[^,]+]] = begin_access [modify] [unknown] [[SELF]]
// CHECK:        [[MODIFY_ACCESSOR:%[^,]+]] = function_ref @$s19coroutine_accessors1SV3irmSivx
// CHECK:        ([[VALUE_ADDRESS:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[MODIFY_ACCESSOR]]([[SELF_ACCESS]])
// CHECK:        assign [[NEW_VALUE:%[^,]+]] to [[VALUE_ADDRESS]]
// CHECK:        end_apply [[TOKEN]]
// CHECK:        end_access [[SELF_ACCESS]]
// CHECK-LABEL:  } // end sil function '$s19coroutine_accessors1SV3irmSivs'
} // public var irm

} // public struct S
