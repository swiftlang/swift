// RUN: %target-swift-emit-silgen-ossa -enable-sil-opaque-values -Xllvm -sil-print-types %s -enable-callee-allocated-coro-abi -enable-library-evolution -enable-experimental-feature CoroutineAccessors
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types   \
// RUN:     %s                                              \
// RUN:     -enable-callee-allocated-coro-abi               \
// RUN:     -enable-library-evolution                       \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN: | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi-stability

// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types   \
// RUN:     %s                                              \
// RUN:     -enable-callee-allocated-coro-abi               \
// RUN:     -enable-library-evolution                       \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -enable-experimental-feature CoroutineAccessorsUnwindOnCallerError \
// RUN: | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi-stability

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: swift_feature_CoroutineAccessorsUnwindOnCallerError

// The old yield_once `_read`/`_modify` accessors (Sivr/SivM below) are
// additively emitted for this resilient module's public storage only on an
// ABI-stable platform (CHECK-stable); elsewhere there is no prebuilt binary to
// stay compatible with, so only the new yield_once_2 accessors are emitted.

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
  yielding borrow {
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
  yielding mutate {
    yield &_i
  }
// CHECK-stable-LABEL: sil{{.*}} [ossa] @$s19coroutine_accessors1SV3irmSivr :
// CHECK-stable-SAME:      $@yield_once
// CHECK-stable-SAME:      @convention(method)
// CHECK-stable-SAME:      (@guaranteed S)
// CHECK-stable-SAME:      ->
// CHECK-stable-SAME:      @yields Int
// CHECK-stable-SAME:  {
// CHECK-stable:       bb0(
// CHECK-stable:           [[SELF:%[^,]+]] :
// CHECK-stable:       ):
// CHECK-stable:         [[READER2:%[^,]+]] = function_ref @$s19coroutine_accessors1SV3irmSivy
// CHECK-stable:         ([[VALUE_ADDRESS:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]+]]) = begin_apply [[READER2]]([[SELF]])
// CHECK-stable:         end_apply [[TOKEN]]
// CHECK-stable:         yield [[VALUE_ADDRESS]] : $Int, resume bb1, unwind bb2
// CHECK-stable:       bb1:
// CHECK-stable:         dealloc_stack [[ALLOCATION]] : $*Builtin.SILToken
// CHECK-stable:       bb2:
// CHECK-stable:         dealloc_stack [[ALLOCATION]] : $*Builtin.SILToken
// CHECK-stable:         unwind
// CHECK-stable-LABEL: } // end sil function '$s19coroutine_accessors1SV3irmSivr'

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

// CHECK-stable-LABEL: sil {{.*}}[ossa] @$s19coroutine_accessors1SV3irmSivM :
// CHECK-stable-SAME:      $@yield_once
// CHECK-stable-SAME:      @convention(method)
// CHECK-stable-SAME:      (@inout S)
// CHECK-stable-SAME:      ->
// CHECK-stable-SAME:      @yields @inout Int
// CHECK-stable-SAME:  {
// CHECK-stable:       bb0(
// CHECK-stable-SAME:      [[SELF:%[^,]+]] :
// CHECK-stable-SAME:  ):
// CHECK-stable:       [[SELF_ACCESS:%[^,]+]] = begin_access [modify] [unknown] [[SELF]]
// CHECK-stable:       [[MODIFY_ACCESSOR:%[^,]+]] = function_ref @$s19coroutine_accessors1SV3irmSivx
// CHECK-stable:       ([[VALUE_ADDRESS:%[^,]+]],
// CHECK-stable-SAME:   [[TOKEN:%[^,]+]],
// CHECK-stable-SAME:   [[ALLOCATION:%[^)]+]])
// CHECK-stable-SAME:  = begin_apply [[MODIFY_ACCESSOR]]([[SELF_ACCESS]])
// CHECK-stable:       yield [[VALUE_ADDRESS]]
// CHECK-stable-SAME:      resume [[RESUME_BB:bb[0-9]+]]
// CHECK-stable-SAME:      unwind [[UNWIND_BB:bb[0-9]+]]
// CHECK-stable:     [[RESUME_BB]]:
// CHECK-stable:       end_apply [[TOKEN]]
// CHECK-stable:       end_access [[SELF_ACCESS]]
// CHECK-stable:       dealloc_stack [[ALLOCATION]]
// CHECK-stable:     [[UNWIND_BB]]:
// CHECK-stable:       end_apply [[TOKEN]]
// CHECK-stable:       dealloc_stack [[ALLOCATION]]
// CHECK-stable:       end_access [[SELF_ACCESS]]
// CHECK-stable:       unwind
// CHECK-stable-LABEL: } // end sil function '$s19coroutine_accessors1SV3irmSivM'
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
// CHECK:      end_apply [[TOKEN]]
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
  _read {
    yield _i
  }
  _modify {
    yield &_i
  }
}

// With the CoroutineAccessors feature enabled, `_read`/`_modify` are just a
// spelling of the yield_once_2 coroutine accessors, so they use the same ABI as
// `yielding borrow`/`yielding mutate`: the yield_once_2 accessors are the primary
// implementation and are emitted first, and (because this module is resilient
// and, for Sivr/SivM, on an ABI-stable platform) the old yield_once accessors
// are also emitted additively.

// CHECK-LABEL: sil {{.*}} @$s19coroutine_accessors1SV5i_r_mSivy : $@yield_once_2 @convention(method) (@guaranteed S) -> @yields Int {

// CHECK-LABEL: sil {{.*}} @$s19coroutine_accessors1SV5i_r_mSivx : $@yield_once_2 @convention(method) (@inout S) -> @yields @inout Int {

// CHECK-stable-LABEL: sil{{.*}} [ossa] @$s19coroutine_accessors1SV5i_r_mSivr :
// CHECK-stable-SAME:      $@yield_once
// CHECK-stable-SAME:      @convention(method)
// CHECK-stable-SAME:      (@guaranteed S)
// CHECK-stable-SAME:      ->
// CHECK-stable-SAME:      @yields Int
// CHECK-stable-SAME:  {
// CHECK-stable:       } // end sil function '$s19coroutine_accessors1SV5i_r_mSivr'

// The synthesized setter forwards to the yield_once_2 modify accessor.
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
// CHECK:         [[MODIFY_ACCESSOR:%[^,]+]] = function_ref @$s19coroutine_accessors1SV5i_r_mSivx
// CHECK:         ([[VALUE_ADDRESS:%[^,]+]],
// CHECK-SAME:     [[TOKEN:%[^,]+]],
// CHECK-SAME:     [[ALLOCATION:%[^)]+]])
// CHECK-SAME:    = begin_apply [[MODIFY_ACCESSOR]]([[SELF_ACCESS]])
// CHECK:         assign [[NEW_VALUE:%[^,]+]] to [[VALUE_ADDRESS]]
// CHECK:         end_apply [[TOKEN]]
// CHECK:         end_access [[SELF_ACCESS]]
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK-LABEL:} // end sil function '$s19coroutine_accessors1SV5i_r_mSivs'

// CHECK-stable-LABEL: sil {{.*}}[ossa] @$s19coroutine_accessors1SV5i_r_mSivM :
// CHECK-stable-SAME:      $@yield_once
// CHECK-stable-SAME:      @convention(method)
// CHECK-stable-SAME:      (@inout S)
// CHECK-stable-SAME:      ->
// CHECK-stable-SAME:      @yields @inout Int
// CHECK-stable-SAME:  {
// CHECK-stable:       } // end sil function '$s19coroutine_accessors1SV5i_r_mSivM'

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
    yielding borrow {
      yield _title
    }
  }
}

protocol GettableTitle {
  var title: String { get }
}

// CHECK-LABEL: sil{{.*}} [ossa] @$s19coroutine_accessors17OverridableReaderC5titleSSvg : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[READER:%[^,]+]] = function_ref @$s19coroutine_accessors17OverridableReaderC5titleSSvy
// CHECK:         ([[TITLE:%[^,]+]], [[TOKEN:%[^,]+]], [[ALLOCATION:%[^,]]]) = begin_apply [[READER]]([[SELF]])
// CHECK:         [[RETVAL:%[^,]+]] = copy_value [[TITLE]]
// CHECK:         end_apply [[TOKEN]] as $()
// CHECK:         dealloc_stack [[ALLOCATION]]
// CHECK:         return [[RETVAL]]
// CHECK-LABEL: } // end sil function '$s19coroutine_accessors17OverridableReaderC5titleSSvg'
// CHECK-LABEL: sil{{.*}} [ossa] @$s19coroutine_accessors17OverridableReaderCAA13GettableTitleA2aDP5titleSSvgTW : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF_ADDR:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[SELF:%[^,]+]] = load_borrow [[SELF_ADDR]]
// CHECK:         [[GETTER:%[^,]+]] = class_method [[SELF]] : $OverridableReader, #OverridableReader.title!getter
// CHECK:         [[RETVAL:%[^,]+]] = apply [[GETTER]]([[SELF]])
// CHECK:         end_borrow [[SELF]]
// CHECK:         return [[RETVAL]]
// CHECK-LABEL: } // end sil function '$s19coroutine_accessors17OverridableReaderCAA13GettableTitleA2aDP5titleSSvgTW'

// CHECK-LABEL:      sil_vtable C {
// CHECK-NEXT:   #C.init!allocator
// CHECK-NEXT:   #C.deinit!deallocator
// CHECK-NEXT: }

public class C {
  final public var i: Int = 0
}

// CHECK-LABEL: sil_witness_table{{.*}} OverridableReader: GettableTitle {{.*}} {
// CHECK-NEXT:    method #GettableTitle.title!getter
// CHECK-SAME:        @$s19coroutine_accessors17OverridableReaderCAA13GettableTitleA2aDP5titleSSvgTW
// CHECK-NEXT:  }
class OverridableReader : GettableTitle {
  var _title: String = ""
  var title: String {
    yielding borrow {
      yield _title
    }
  }
}

// CHECK-LABEL: sil_default_witness_table ReadableField {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #ReadableField.field!yielding_borrow
// CHECK-SAME:        : @$s19coroutine_accessors13ReadableFieldP5fieldSivy
// CHECK-NEXT:  }
public protocol ReadableField {
  @_borrowed
  var field: Int { get }
}
