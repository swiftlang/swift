// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types   \
// RUN:     %s                                              \
// RUN:     -enable-callee-allocated-coro-abi               \
// RUN:     -enable-library-evolution                       \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN: | %FileCheck %s --check-prefixes=CHECK,CHECK-NOUNWIND

// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types                      \
// RUN:     %s                                                                 \
// RUN:     -enable-callee-allocated-coro-abi                                  \
// RUN:     -enable-library-evolution                                          \
// RUN:     -enable-experimental-feature CoroutineAccessors                    \
// RUN:     -enable-experimental-feature CoroutineAccessorsUnwindOnCallerError \
// RUN: | %FileCheck %s --check-prefixes=CHECK,CHECK-UNWIND

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: swift_feature_CoroutineAccessorsUnwindOnCallerError

@frozen
public struct S {
public var o: any AnyObject
public var _i: Int = 0

public var irm: Int {
// CHECK-LABEL: sil [ossa] @$s19coroutine_accessors1SV3irmSivy :
// CHECK-SAME:      $@yield_once_2
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
// CHECK-SAME:      $@yield_once_2
// CHECK-SAME:      @convention(method)
// CHECK-SAME:      (@inout S)
// CHECK-SAME:      ->
// CHECK-SAME:      @yields @inout Int
// CHECK-SAME:  {
// CHECK-LABEL: } // end sil function '$s19coroutine_accessors1SV3irmSivx'
  modify {
    yield &_i
  }
// CHECK-LABEL: sil{{.*}} [ossa] @$s19coroutine_accessors1SV3irmSivr :
// CHECK-SAME:      $@yield_once
// CHECK-SAME:      @convention(method)
// CHECK-SAME:      (@guaranteed S)
// CHECK-SAME:      ->
// CHECK-SAME:      @yields Int
// CHECK-SAME:  {
// CHECK:       bb0(
// CHECK:           [[SELF:%[^,]+]] :
// CHECK:       ):
// CHECK:         [[READER2:%[^,]+]] = function_ref @$s19coroutine_accessors1SV3irmSivy
// CHECK:         ([[VALUE_ADDRESS:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READER2]]([[SELF]])
// CHECK:         end_apply [[TOKEN]]
// CHECK:         yield [[VALUE_ADDRESS]] : $Int, resume bb1, unwind bb2
// CHECK:       bb1:
// CHECK:         dealloc_stack [[ALLOCATION]] : $*Builtin.SILToken
// CHECK:       bb2:
// CHECK:         dealloc_stack [[ALLOCATION]] : $*Builtin.SILToken
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s19coroutine_accessors1SV3irmSivr'

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
// CHECK:       yield [[VALUE_ADDRESS]] 
// CHECK-SAME:      resume [[RESUME_BB:bb[0-9]+]]
// CHECK-SAME:      unwind [[UNWIND_BB:bb[0-9]+]]
// CHECK:     [[RESUME_BB]]:
// CHECK:       end_apply [[TOKEN]]
// CHECK:       end_access [[SELF_ACCESS]]
// CHECK:       dealloc_stack [[ALLOCATION]]
// CHECK:     [[UNWIND_BB]]:
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

public var i_r_m: Int {
// CHECK-LABEL: sil{{.*}} [ossa] @$s19coroutine_accessors1SV5i_r_mSivr :
// CHECK-SAME:      $@yield_once
// CHECK-SAME:      @convention(method)
// CHECK-SAME:      (@guaranteed S)
// CHECK-SAME:      ->
// CHECK-SAME:      @yields Int
// CHECK-SAME:  {
// CHECK:       } // end sil function '$s19coroutine_accessors1SV5i_r_mSivr'

  _read {
    yield _i
  }
// CHECK-NOT:   sil [ossa] @$s19coroutine_accessors1SV5i_r_mSivx :
// CHECK-LABEL: sil {{.*}}[ossa] @$s19coroutine_accessors1SV5i_r_mSivM :
// CHECK-SAME:      $@yield_once
// CHECK-SAME:      @convention(method)
// CHECK-SAME:      (@inout S)
// CHECK-SAME:      ->
// CHECK-SAME:      @yields @inout Int
// CHECK-SAME:  {
// CHECK:       } // end sil function '$s19coroutine_accessors1SV5i_r_mSivM'
  _modify {
    yield &_i
  }
// CHECK-LABEL: sil {{.*}}[ossa] @$s19coroutine_accessors1SV5i_r_mSivs :
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
// CHECK:         [[MODIFY_ACCESSOR:%[^,]+]] = function_ref @$s19coroutine_accessors1SV5i_r_mSivM
// CHECK:         ([[VALUE_ADDRESS:%[^,]+]],
// CHECK-SAME:     [[TOKEN:%[^)]+]])
// CHECK-SAME:    = begin_apply [[MODIFY_ACCESSOR]]([[SELF_ACCESS]])
// CHECK:         assign [[NEW_VALUE:%[^,]+]] to [[VALUE_ADDRESS]]
// CHECK:         end_apply [[TOKEN]]
// CHECK:         end_access [[SELF_ACCESS]]
// CHECK-LABEL:} // end sil function '$s19coroutine_accessors1SV5i_r_mSivs'
} // public var irm

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

protocol ReadableTitle {
  var title: String { read }
}
class OverridableGetter : ReadableTitle {
  var title: String = ""
}
//   The read witness thunk does a direct call to the concrete read accessor.
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s19coroutine_accessors17OverridableGetterCAA13ReadableTitleA2aDP5titleSSvyTW
// CHECK:       function_ref @$s19coroutine_accessors17OverridableGetterC5titleSSvy
// CHECK-LABEL: // end sil function '$s19coroutine_accessors17OverridableGetterCAA13ReadableTitleA2aDP5titleSSvyTW'
//   The concrete read accessor is generated on-demand and does a class dispatch to the getter.
// CHECK-LABEL: sil shared [ossa] @$s19coroutine_accessors17OverridableGetterC5titleSSvy
// CHECK:       class_method %0 : $OverridableGetter, #OverridableGetter.title!getter
// CHECK-LABEL: // end sil function '$s19coroutine_accessors17OverridableGetterC5titleSSvy'

class ImplementedReader : ReadableTitle {
  var _title: String = ""
  var title: String {
    read {
      yield _title
    }
  }
}

protocol GettableTitle {
  var title: String { get }
}

// CHECK-LABEL: sil{{.*}} [ossa] @$s19coroutine_accessors17OverridableReaderCAA13GettableTitleA2aDP5titleSSvgTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load_borrow [[SELF_ADDR]]
// CHECK:         [[GETTER:%[^,]+]] = function_ref @$s19coroutine_accessors17OverridableReaderC5titleSSvg
// CHECK:         [[RETVAL:%[^,]+]] = apply [[GETTER]]([[SELF]])
// CHECK:         end_borrow [[SELF]]
// CHECK:         return [[RETVAL]]
// CHECK-LABEL: } // end sil function '$s19coroutine_accessors17OverridableReaderCAA13GettableTitleA2aDP5titleSSvgTW'
// CHECK-LABEL: sil{{.*}} [ossa] @$s19coroutine_accessors17OverridableReaderC5titleSSvg : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[READER:%[^,]+]] = class_method [[SELF]] : $OverridableReader, #OverridableReader.title!read2
// CHECK:         ([[TITLE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]]]) = begin_apply [[READER]]([[SELF]])
// CHECK:         [[RETVAL:%[^,]+]] = copy_value [[TITLE]]
// CHECK:         end_apply [[TOKEN]] as $()
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         return [[RETVAL]]
// CHECK-LABEL: } // end sil function '$s19coroutine_accessors17OverridableReaderC5titleSSvg'

// CHECK-LABEL: sil_witness_table{{.*}} OverridableReader: GettableTitle {{.*}} {
// CHECK-NEXT:    method #GettableTitle.title!getter
// CHECK-SAME:        @$s19coroutine_accessors17OverridableReaderCAA13GettableTitleA2aDP5titleSSvgTW
// CHECK-NEXT:  }
class OverridableReader : GettableTitle {
  var _title: String = ""
  var title: String {
    read {
      yield _title
    }
  }
}

// CHECK-LABEL: sil_default_witness_table ReadableField {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ReadableField.field!read2 
// CHECK-SAME:        : @$s19coroutine_accessors13ReadableFieldP5fieldSivy
// CHECK-NEXT:  }
public protocol ReadableField {
  @_borrowed
  var field: Int { get }
}
