// RUN: %target-swift-emit-silgen                           \
// RUN:     %s                                              \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN: | %FileCheck %s --check-prefixes=CHECK,CHECK-NOUNWIND

// RUN: %target-swift-emit-silgen                                              \
// RUN:     %s                                                                 \
// RUN:     -enable-experimental-feature CoroutineAccessors                    \
// RUN:     -enable-experimental-feature CoroutineAccessorsUnwindOnCallerError \
// RUN: | %FileCheck %s --check-prefixes=CHECK,CHECK-UNWIND

// REQUIRES: asserts

public struct S {
public var o: any AnyObject
public var _i: Int = 0

public var irm: Int {
// CHECK-LABEL: sil [ossa] @$s19coroutine_accessors1SV3irmSivy :
// CHECK-SAME:      $@yield_once
// CHECK-SAME:      @convention(method)
// CHECK-SAME:      (@guaranteed S)
// CHECK-SAME:      ->
// CHECK-SAME:      @yields Int
// CHECK-SAME:  {
// CHECK-LABEL: } // end sil function '$s19coroutine_accessors1SV3irmSivy'
  read {
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
// CHECK-LABEL: sil {{.*}}[ossa] @$s19coroutine_accessors1SV3irmSivg :
// CHECK-SAME:      $@convention(method)
// CHECK-SAME:      (@guaranteed S)
// CHECK-SAME:      ->
// CHECK-SAME:      Int
// CHECK-SAME:  {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[READ_ACCESSOR:%[^,]+]] = function_ref @$s19coroutine_accessors1SV3irmSivy
// CHECK:         ([[VALUE:%[^,]+]],
// CHECK-SAME:     [[TOKEN:%[^,]+]],
// CHECK-SAME:     [[ALLOCATION:%[^)]+]])
// CHECK-SAME:    = begin_apply [[READ_ACCESSOR]]([[SELF]])
// CHECK:         end_apply [[TOKEN]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         return [[VALUE:%[^,]+]]
// CHECK-LABEL: } // end sil function '$s19coroutine_accessors1SV3irmSivg'

// CHECK-LABEL: sil {{.*}}[ossa] @$s19coroutine_accessors1SV3irmSivs :
// CHECK-SAME:      $@convention(method)
// CHECK-SAME:      (Int, @inout S)
// CHECK-SAME:      ->
// CHECK-SAME:      ()
// CHECK-SAME:  {
// CHECK:       bb0(
// CHECK-SAME:      [[NEW_VALUE:%[^,]+]] :
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[SELF_ACCESS:%[^,]+]] = begin_access [modify] [unknown] [[SELF]]
// CHECK:         [[MODIFY_ACCESSOR:%[^,]+]] = function_ref @$s19coroutine_accessors1SV3irmSivx
// CHECK:         ([[VALUE_ADDRESS:%[^,]+]],
// CHECK-SAME:     [[TOKEN:%[^,]+]],
// CHECK-SAME:     [[ALLOCATION:%[^)]+]])
// CHECK-SAME:    = begin_apply [[MODIFY_ACCESSOR]]([[SELF_ACCESS]])
// CHECK:         assign [[NEW_VALUE:%[^,]+]] to [[VALUE_ADDRESS]]
// CHECK:         end_apply [[TOKEN]]
// CHECK:         end_access [[SELF_ACCESS]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK-LABEL:} // end sil function '$s19coroutine_accessors1SV3irmSivs'

// CHECK-LABEL: sil {{.*}}[ossa] @$s19coroutine_accessors1SV3irmSivM :
// CHECK-SAME:      $@yield_once
// CHECK-SAME:      @convention(method)
// CHECK-SAME:      (@inout S)
// CHECK-SAME:      ->
// CHECK-SAME:      @yields @inout Int
// CHECK-SAME:  {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:       [[SELF_ACCESS:%[^,]+]] = begin_access [modify] [unknown] [[SELF]]
// CHECK:       [[MODIFY_ACCESSOR:%[^,]+]] = function_ref @$s19coroutine_accessors1SV3irmSivx
// CHECK:       ([[VALUE_ADDRESS:%[^,]+]],
// CHECK-SAME:   [[TOKEN:%[^,]+]],
// CHECK-SAME:   [[ALLOCATION:%[^)]+]])
// CHECK-SAME:  = begin_apply [[MODIFY_ACCESSOR]]([[SELF_ACCESS]])
// CHECK:       yield [[VALUE_ADDRESS:%[^,]+]] : $*Int, resume bb1, unwind bb2
// CHECK:     bb1:
// CHECK:       end_apply [[TOKEN]]
// CHECK:       end_access [[SELF_ACCESS]]
// CHECK:       dealloc_stack [[ALLOCATION]]
// CHECK:     bb2:
// CHECK:       end_apply [[TOKEN]]
// CHECK:       dealloc_stack [[ALLOCATION]]
// CHECK:       end_access [[SELF_ACCESS]]
// CHECK:       unwind
// CHECK-LABEL: } // end sil function '$s19coroutine_accessors1SV3irmSivM'
} // public var irm

// CHECK-LABEL: sil {{.*}}[ossa] @$s19coroutine_accessors1SV6update3irmS2i_tKF :
// CHECK-SAME:      $@convention(method)
// CHECK-SAME:      (Int, @inout S)
// CHECK-SAME:      ->
// CHECK-SAME:      (Int, @error any Error)
// CHECK-SAME: {
// CHECK:      bb0(
// CHECK-SAME:      [[NEW_VALUE:%[^,]+]] :
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME: ):
// CHECK:      [[OLD_VALUE_ADDR:%[^,]+]] = alloc_stack $Int
// CHECK:      [[NEW_VALUE_ADDR:%[^,]+]] = alloc_stack $Int
// CHECK:      store [[NEW_VALUE:%[^,]+]] to [trivial] [[NEW_VALUE_ADDR]]
// CHECK:      [[SELF_ACCESS:%[^,]+]] = begin_access [modify] [unknown] [[SELF]]
// CHECK:      [[MODIFY_ACCESSOR:%[^,]+]] = function_ref @$s19coroutine_accessors1SV3irmSivx
// CHECK:      ([[VALUE_ADDR:%[^,]+]], 
// CHECK-SAME:  [[TOKEN:%[^,]+]],
// CHECK-SAME:  [[ALLOCATION:%[^)]+]])
// CHECK-SAME: = begin_apply [[MODIFY_ACCESSOR]]([[SELF_ACCESS]])
// CHECK:      [[UPDATE:%[^,]+]] = function_ref @$s19coroutine_accessors6update2at2toxxz_xtKSQRzlF
// CHECK:      try_apply [[UPDATE:%[^,]+]]<Int>([[OLD_VALUE_ADDR]], [[VALUE_ADDR]], [[NEW_VALUE_ADDR]])
// CHECK:    bb1
// CHECK:      end_apply [[TOKEN]] as $()
// CHECK:      end_access [[SELF_ACCESS]]
// CHECK:      dealloc_stack [[ALLOCATION]]
// CHECK:      dealloc_stack [[NEW_VALUE_ADDR]]
// CHECK:      [[OLD_VALUE:%[^,]+]] = load [trivial] [[OLD_VALUE_ADDR]]
// CHECK:      dealloc_stack [[OLD_VALUE_ADDR]]
// CHECK:      return [[OLD_VALUE]]
// CHECK:    bb2([[ERROR:%[^,]+]] : @owned $any Error):
// CHECK-NOUNWIND: end_apply [[TOKEN]]
// CHECK-UNWIND: abort_apply [[TOKEN]]
// CHECK:      dealloc_stack [[ALLOCATION]]
// CHECK:      end_access [[SELF_ACCESS]]
// CHECK:      dealloc_stack [[NEW_VALUE_ADDR]]
// CHECK:      dealloc_stack [[OLD_VALUE_ADDR]]
// CHECK:      throw [[ERROR]]
// CHECK-LABEL: } // end sil function '$s19coroutine_accessors1SV6update3irmS2i_tKF'
mutating func update(irm newValue: Int) throws -> Int {
  try coroutine_accessors.update(at: &irm, to: newValue)
}

} // public struct S

enum E : Error {
  case e
}

func update<T : Equatable>(at location: inout T, to newValue: T) throws -> T {
  let oldValue = location
  if oldValue == newValue {
    throw E.e
  }
  location = newValue
  return oldValue
}
