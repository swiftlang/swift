// RUN: %target-swift-emit-silgen                           \
// RUN:     %s                                              \
// RUN:     -enable-callee-allocated-coro-abi               \
// RUN:     -enable-library-evolution                       \
// RUN:     -enable-experimental-feature BuiltinModule      \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN: | %FileCheck %s

// RUN: %target-swift-emit-silgen                           \
// RUN:     %s                                              \
// RUN:     -enable-callee-allocated-coro-abi               \
// RUN:     -enable-experimental-feature BuiltinModule      \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN: | %FileCheck %s --check-prefix=CHECK-FRAGILE

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: swift_feature_BuiltinModule

import Builtin

public enum Open {}
public enum Public {}
public enum Internal {}

open class OpenBase<T, U> {
  @_borrowed
  open var openField: T {
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC9openFieldxvyTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1> 
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @in_guaranteed τ_0_0
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.openField!read
// CHECK-SAME:        <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> @yields @in_guaranteed τ_0_0
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK-SAME:        resume [[NORMAL:bb[0-9]+]]
// CHECK-SAME:        unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC9openFieldxvyTwd'
    read {
      Builtin.int_trap()
    }
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC9openFieldxvxTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @inout τ_0_0
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.openField!modify
// CHECK:             <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK:             $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> @yields @inout τ_0_0
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK:             resume [[NORMAL:bb[0-9]+]]
// CHECK:             unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC9openFieldxvxTwd'
    modify {
      Builtin.int_trap()
    }
  }
  @_borrowed
  open var openField2: (T, T) {
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC10openField2x_xtvyTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1> 
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @in_guaranteed τ_0_0
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.openField2!read
// CHECK-SAME:        <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> (@yields @in_guaranteed τ_0_0, @yields @in_guaranteed τ_0_0)
// CHECK:         ([[ADDR_1:%[^,]+]], [[ADDR_2:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield ([[ADDR_1]], [[ADDR_2]])
// CHECK-SAME:        resume [[NORMAL:bb[0-9]+]]
// CHECK-SAME:        unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC10openField2x_xtvyTwd'
    read {
      Builtin.int_trap()
    }
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC10openField2x_xtvxTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @inout (τ_0_0, τ_0_0)
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.openField2!modify
// CHECK:             <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK:             $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> @yields @inout (τ_0_0, τ_0_0)
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK:             resume [[NORMAL:bb[0-9]+]]
// CHECK:             unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC10openField2x_xtvxTwd'
    modify {
      Builtin.int_trap()
    }
  }
  @_borrowed
  open var openField3: (T, Int) {
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC10openField3x_SitvyTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1> 
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @in_guaranteed τ_0_0
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.openField3!read
// CHECK-SAME:        <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> (@yields @in_guaranteed τ_0_0, @yields Int)
// CHECK:         ([[ADDR_1:%[^,]+]], [[ADDR_2:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield ([[ADDR_1]], [[ADDR_2]])
// CHECK-SAME:        resume [[NORMAL:bb[0-9]+]]
// CHECK-SAME:        unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC10openField3x_SitvyTwd'
    read {
      Builtin.int_trap()
    }
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC10openField3x_SitvxTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @inout (τ_0_0, Int)
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.openField3!modify
// CHECK:             <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK:             $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> @yields @inout (τ_0_0, Int)
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK:             resume [[NORMAL:bb[0-9]+]]
// CHECK:             unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC10openField3x_SitvxTwd'
    modify {
      Builtin.int_trap()
    }
  }
  @_borrowed
  open var openField4: (String, T) {
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC10openField4SS_xtvyTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1> 
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @in_guaranteed τ_0_0
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.openField4!read
// CHECK-SAME:        <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> (@yields @guaranteed String, @yields @in_guaranteed τ_0_0)
// CHECK:         ([[ADDR_1:%[^,]+]], [[ADDR_2:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield ([[ADDR_1]], [[ADDR_2]])
// CHECK-SAME:        resume [[NORMAL:bb[0-9]+]]
// CHECK-SAME:        unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC10openField4SS_xtvyTwd'
    read {
      Builtin.int_trap()
    }
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC10openField4SS_xtvxTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @inout (String, τ_0_0)
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.openField4!modify
// CHECK:             <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK:             $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> @yields @inout (String, τ_0_0)
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK:             resume [[NORMAL:bb[0-9]+]]
// CHECK:             unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC10openField4SS_xtvxTwd'
    modify {
      Builtin.int_trap()
    }
  }
  @_borrowed
  open var openField5: (T, U) {
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC10openField5x_q_tvyTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1> 
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         (@yields @in_guaranteed τ_0_0, @yields @in_guaranteed τ_0_1)
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.openField5!read
// CHECK-SAME:        <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> (@yields @in_guaranteed τ_0_0, @yields @in_guaranteed τ_0_1)
// CHECK:         ([[ADDR_1:%[^,]+]], [[ADDR_2:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield ([[ADDR_1]], [[ADDR_2]])
// CHECK-SAME:        resume [[NORMAL:bb[0-9]+]]
// CHECK-SAME:        unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC10openField5x_q_tvyTwd'
    read {
      Builtin.int_trap()
    }
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC10openField5x_q_tvxTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @inout (τ_0_0, τ_0_1)
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.openField5!modify
// CHECK:             <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK:             $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> @yields @inout (τ_0_0, τ_0_1)
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK:             resume [[NORMAL:bb[0-9]+]]
// CHECK:             unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC10openField5x_q_tvxTwd'
    modify {
      Builtin.int_trap()
    }
  }
  open var ownedOpenField: T {
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC05ownedC5FieldxvyTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @in_guaranteed τ_0_0
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.ownedOpenField!getter
// CHECK-SAME:        <T, U> (OpenBase<T, U>) -> () -> T
// CHECK-SAME:        $@convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> @out τ_0_0
// CHECK:         [[ADDR:%[^,]+]] = alloc_stack
// CHECK:         [[EMPTY:%[^,]+]] = apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[ADDR]], [[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK-SAME:        resume [[NORMAL:bb[0-9]+]]
// CHECK-SAME:        unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         destroy_addr [[ADDR]]
// CHECK:         dealloc_stack [[ADDR]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         destroy_addr [[ADDR]]
// CHECK:         dealloc_stack [[ADDR]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC05ownedC5FieldxvyTwd'
    read {
      Builtin.int_trap()
    }
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC05ownedC5FieldxvxTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @inout τ_0_0
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.ownedOpenField!modify
// CHECK:             <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK:             $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> @yields @inout τ_0_0
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK:             resume [[NORMAL:bb[0-9]+]]
// CHECK:             unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC05ownedC5FieldxvxTwd'
    modify {
      Builtin.int_trap()
    }
  }
  open var ownedOpenField2: (T, T) {
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC05ownedC6Field2x_xtvyTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         (@yields @in_guaranteed τ_0_0, @yields @in_guaranteed τ_0_0)
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.ownedOpenField2!getter
// CHECK-SAME:        <T, U> (OpenBase<T, U>) -> () -> (T, T)
// CHECK-SAME:        $@convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> (@out τ_0_0, @out τ_0_0)
// CHECK:         [[ADDR_1:%[^,]+]] = alloc_stack
// CHECK:         [[ADDR_2:%[^,]+]] = alloc_stack
// CHECK:         [[EMPTY:%[^,]+]] = apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[ADDR_1]], [[ADDR_2]], [[SELF]])
// CHECK:         yield ([[ADDR_1]], [[ADDR_2]])
// CHECK-SAME:        resume [[NORMAL:bb[0-9]+]]
// CHECK-SAME:        unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         destroy_addr [[ADDR_2]]
// CHECK:         dealloc_stack [[ADDR_2]]
// CHECK:         destroy_addr [[ADDR_1]]
// CHECK:         dealloc_stack [[ADDR_1]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         destroy_addr [[ADDR_2]]
// CHECK:         dealloc_stack [[ADDR_2]]
// CHECK:         destroy_addr [[ADDR_1]]
// CHECK:         dealloc_stack [[ADDR_1]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC05ownedC6Field2x_xtvyTwd'
    read {
      Builtin.int_trap()
    }
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC05ownedC6Field2x_xtvxTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @inout (τ_0_0, τ_0_0)
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.ownedOpenField2!modify
// CHECK:             <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK:             $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> @yields @inout (τ_0_0, τ_0_0)
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK:             resume [[NORMAL:bb[0-9]+]]
// CHECK:             unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC05ownedC6Field2x_xtvxTwd'
    modify {
      Builtin.int_trap()
    }
  }
  open var ownedOpenField3: (T, Int) {
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC05ownedC6Field3x_SitvyTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @in_guaranteed τ_0_0
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.ownedOpenField3!getter
// CHECK-SAME:        <T, U> (OpenBase<T, U>) -> () -> (T, Int)
// CHECK-SAME:        $@convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> (@out τ_0_0, Int)
// CHECK:         [[ADDR:%[^,]+]] = alloc_stack
// CHECK:         [[INT:%[^,]+]] = apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[ADDR]], [[SELF]])
// CHECK:         yield ([[ADDR]], [[INT]])
// CHECK-SAME:        resume [[NORMAL:bb[0-9]+]]
// CHECK-SAME:        unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         destroy_addr [[ADDR]]
// CHECK:         dealloc_stack [[ADDR]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         destroy_addr [[ADDR]]
// CHECK:         dealloc_stack [[ADDR]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC05ownedC6Field3x_SitvyTwd'
    read {
      Builtin.int_trap()
    }
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC05ownedC6Field3x_SitvxTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @inout (τ_0_0, Int)
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.ownedOpenField3!modify
// CHECK:             <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK:             $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> @yields @inout (τ_0_0, Int)
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK:             resume [[NORMAL:bb[0-9]+]]
// CHECK:             unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC05ownedC6Field3x_SitvxTwd'
    modify {
      Builtin.int_trap()
    }
  }
  open var ownedOpenField4: (String, T) {
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC05ownedC6Field4SS_xtvyTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         (@yields @guaranteed String, @yields @in_guaranteed τ_0_0)
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.ownedOpenField4!getter
// CHECK-SAME:        <T, U> (OpenBase<T, U>) -> () -> (String, T)
// CHECK-SAME:        $@convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> (@owned String, @out τ_0_0)
// CHECK:         [[ADDR:%[^,]+]] = alloc_stack
// CHECK:         [[STRING:%[^,]+]] = apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[ADDR]], [[SELF]])
// CHECK:         yield ([[STRING]], [[ADDR]])
// CHECK-SAME:        resume [[NORMAL:bb[0-9]+]]
// CHECK-SAME:        unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         destroy_addr [[ADDR]]
// CHECK:         dealloc_stack [[ADDR]]
// CHECK:         destroy_value [[STRING]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         destroy_addr [[ADDR]]
// CHECK:         dealloc_stack [[ADDR]]
// CHECK:         destroy_value [[STRING]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC05ownedC6Field4SS_xtvyTwd'
    read {
      Builtin.int_trap()
    }
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC05ownedC6Field4SS_xtvxTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @inout (String, τ_0_0)
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.ownedOpenField4!modify
// CHECK:             <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK:             $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> @yields @inout (String, τ_0_0)
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK:             resume [[NORMAL:bb[0-9]+]]
// CHECK:             unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC05ownedC6Field4SS_xtvxTwd'
    modify {
      Builtin.int_trap()
    }
  }
  open var ownedOpenField5: (T, U) {
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC05ownedC6Field5x_q_tvyTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @in_guaranteed τ_0_0
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.ownedOpenField5!getter
// CHECK-SAME:        <T, U> (OpenBase<T, U>) -> () -> (T, U)
// CHECK-SAME:        $@convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> (@out τ_0_0, @out τ_0_1)
// CHECK:         [[ADDR_1:%[^,]+]] = alloc_stack
// CHECK:         [[ADDR_2:%[^,]+]] = alloc_stack
// CHECK:         [[EMPTY:%[^,]+]] = apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[ADDR_1]], [[ADDR_2]], [[SELF]])
// CHECK:         yield ([[ADDR_1]], [[ADDR_2]])
// CHECK-SAME:        resume [[NORMAL:bb[0-9]+]]
// CHECK-SAME:        unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         destroy_addr [[ADDR_2]]
// CHECK:         dealloc_stack [[ADDR_2]]
// CHECK:         destroy_addr [[ADDR_1]]
// CHECK:         dealloc_stack [[ADDR_1]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         destroy_addr [[ADDR_2]]
// CHECK:         dealloc_stack [[ADDR_2]]
// CHECK:         destroy_addr [[ADDR_1]]
// CHECK:         dealloc_stack [[ADDR_1]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC05ownedC6Field5x_q_tvyTwd'
    read {
      Builtin.int_trap()
    }
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC05ownedC6Field5x_q_tvxTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @inout (τ_0_0, τ_0_1)
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.ownedOpenField5!modify
// CHECK:             <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK:             $@yield_once @convention(method) <τ_0_0, τ_0_1> (@guaranteed OpenBase<τ_0_0, τ_0_1>) -> @yields @inout (τ_0_0, τ_0_1)
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1>([[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK:             resume [[NORMAL:bb[0-9]+]]
// CHECK:             unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseC05ownedC6Field5x_q_tvxTwd'
    modify {
      Builtin.int_trap()
    }
  }
  open subscript<V>(_ v: V, _ _: Open.Type) -> T {
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseCyxqd___AA0C0OmtcluiyTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1><τ_1_0>
// CHECK-SAME:         (
// CHECK-SAME:             @in_guaranteed τ_1_0
// CHECK-SAME:             @thin Open.Type
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @in_guaranteed τ_0_0
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[KEY:%[^,]+]] :
// CHECK-SAME:      [[OPEN_TY:%[^,]+]] :
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]]
// CHECK:             #OpenBase.subscript!getter
// CHECK:             <T, U><V> (OpenBase<T, U>) -> (V, Open.Type) -> T
// CHECK:             $@convention(method) <τ_0_0, τ_0_1><τ_1_0> (@in_guaranteed τ_1_0, @thin Open.Type, @guaranteed OpenBase<τ_0_0, τ_0_1>) -> @out τ_0_0
// CHECK:         [[ADDR:%[^,]+]] = alloc_stack
// CHECK:         [[EMPTY:%[^,]+]] = apply [[ORIGINAL]]<τ_0_0, τ_0_1, τ_1_0>([[ADDR]], [[KEY]], [[OPEN_TY]], [[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK:             resume bb1
// CHECK:             unwind bb2
// CHECK:       bb1:
// CHECK:         destroy_addr [[ADDR]]
// CHECK:         dealloc_stack [[ADDR]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       bb2:
// CHECK:         destroy_addr [[ADDR]]
// CHECK:         dealloc_stack [[ADDR]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseCyxqd___AA0C0OmtcluiyTwd'
    read {
      Builtin.int_trap()
    }
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseCyxqd___AA0C0OmtcluixTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0, τ_0_1><τ_1_0>
// CHECK-SAME:         (
// CHECK-SAME:             @in_guaranteed τ_1_0
// CHECK-SAME:             @thin Open.Type
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0, τ_0_1>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @inout τ_0_0
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[KEY:%[^,]+]] :
// CHECK-SAME:      [[OPEN_TY:%[^,]+]] :
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]]
// CHECK-SAME:        #OpenBase.subscript!modify
// CHECK-SAME:        <T, U><V> (OpenBase<T, U>) -> (V, Open.Type) -> ()
// CHECK-SAME:        $@yield_once @convention(method) <τ_0_0, τ_0_1><τ_1_0> (@in_guaranteed τ_1_0, @thin Open.Type, @guaranteed OpenBase<τ_0_0, τ_0_1>) -> @yields @inout τ_0_0
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_0_1, τ_1_0>([[KEY]], [[OPEN_TY]], [[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK:             resume [[NORMAL:bb[0-9]+]]
// CHECK:             unwind [[UNWIND:bb[0-9]+]]
// CHECK:       [[NORMAL]]:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       [[UNWIND]]:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseCyxqd___AA0C0OmtcluixTwd'
    modify {
      Builtin.int_trap()
    }
  }
  public var publicField: T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  public subscript<V>(_ v: V, _ _: Public.Type) -> T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  var internalField: T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  subscript<V>(_ v: V, _ _: Internal.Type) -> T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
}

public class PublicBase<T, U> {
  open var openField: T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  open subscript<V>(_ v: V, _ _: Open.Type) -> T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  public var publicField: T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  public subscript<V>(_ v: V, _ _: Public.Type) -> T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  var internalField: T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  subscript<V>(_ v: V, _ _: Internal.Type) -> T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
}

class InternalBase<T, U> {
  open var openField: T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  open subscript<V>(_ v: V, _ _: Open.Type) -> T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  public var publicField: T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  public subscript<V>(_ v: V, _ _: Public.Type) -> T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  var internalField: T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  subscript<V>(_ v: V, _ _: Internal.Type) -> T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
}

// CHECK-LABEL: sil_default_override_table OpenBase {
// CHECK-NEXT:    #OpenBase.openField!read2
// CHECK-SAME:        #OpenBase.openField!read
// CHECK-SAME:        <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC9openFieldxvyTwd
// CHECK-NEXT:    #OpenBase.openField!modify2
// CHECK-SAME:        #OpenBase.openField!modify
// CHECK-SAME:        <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC9openFieldxvxTwd
// CHECK-NEXT:    #OpenBase.openField2!read2
// CHECK-SAME:        #OpenBase.openField2!read: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC10openField2x_xtvyTwd
// CHECK-NEXT:    #OpenBase.openField2!modify2
// CHECK-SAME:        #OpenBase.openField2!modify: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC10openField2x_xtvxTwd
// CHECK-NEXT:    #OpenBase.openField3!read2
// CHECK-SAME:        #OpenBase.openField3!read: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC10openField3x_SitvyTwd
// CHECK-NEXT:    #OpenBase.openField3!modify2
// CHECK-SAME:        #OpenBase.openField3!modify: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC10openField3x_SitvxTwd
// CHECK-NEXT:    #OpenBase.openField4!read2
// CHECK-SAME:        #OpenBase.openField4!read: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC10openField4SS_xtvyTwd
// CHECK-NEXT:    #OpenBase.openField4!modify2
// CHECK-SAME:        #OpenBase.openField4!modify: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC10openField4SS_xtvxTwd
// CHECK-NEXT:    #OpenBase.openField5!read2
// CHECK-SAME:        #OpenBase.openField5!read: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC10openField5x_q_tvyTwd
// CHECK-NEXT:    #OpenBase.openField5!modify2
// CHECK-SAME:        #OpenBase.openField5!modify: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC10openField5x_q_tvxTwd
// CHECK-NEXT:    #OpenBase.ownedOpenField!read2
// CHECK-SAME:        #OpenBase.ownedOpenField!getter: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC05ownedC5FieldxvyTwd
// CHECK-NEXT:    #OpenBase.ownedOpenField!modify2
// CHECK-SAME:        #OpenBase.ownedOpenField!modify: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC05ownedC5FieldxvxTwd
// CHECK-NEXT:    #OpenBase.ownedOpenField2!read2
// CHECK-SAME:        #OpenBase.ownedOpenField2!getter: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC05ownedC6Field2x_xtvyTwd
// CHECK-NEXT:    #OpenBase.ownedOpenField2!modify2
// CHECK-SAME:        #OpenBase.ownedOpenField2!modify: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC05ownedC6Field2x_xtvxTwd
// CHECK-NEXT:    #OpenBase.ownedOpenField3!read2
// CHECK-SAME:        #OpenBase.ownedOpenField3!getter: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC05ownedC6Field3x_SitvyTwd
// CHECK-NEXT:    #OpenBase.ownedOpenField3!modify2
// CHECK-SAME:        #OpenBase.ownedOpenField3!modify: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC05ownedC6Field3x_SitvxTwd
// CHECK-NEXT:    #OpenBase.ownedOpenField4!read2
// CHECK-SAME:        #OpenBase.ownedOpenField4!getter: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC05ownedC6Field4SS_xtvyTwd
// CHECK-NEXT:    #OpenBase.ownedOpenField4!modify2
// CHECK-SAME:        #OpenBase.ownedOpenField4!modify: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC05ownedC6Field4SS_xtvxTwd
// CHECK-NEXT:    #OpenBase.ownedOpenField5!read2
// CHECK-SAME:        #OpenBase.ownedOpenField5!getter: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC05ownedC6Field5x_q_tvyTwd
// CHECK-NEXT:    #OpenBase.ownedOpenField5!modify2
// CHECK-SAME:        #OpenBase.ownedOpenField5!modify: <T, U> (OpenBase<T, U>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC05ownedC6Field5x_q_tvxTwd
// CHECK-NEXT:    #OpenBase.subscript!read2
// CHECK-SAME:        #OpenBase.subscript!getter
// CHECK-SAME:        <T, U><V> (OpenBase<T, U>) -> (V, Open.Type) -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseCyxqd___AA0C0OmtcluiyTwd
// CHECK-NEXT:    #OpenBase.subscript!modify2
// CHECK-SAME:        #OpenBase.subscript!modify
// CHECK-SAME:        <T, U><V> (OpenBase<T, U>) -> (V, Open.Type) -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseCyxqd___AA0C0OmtcluixTwd
// CHECK-NOT:     #OpenBase.publicField!read2
// CHECK-NOT:     #OpenBase.publicField!modify2
// CHECK-NOT:     #OpenBase.subscript!read2: #OpenBase.subscript!read: <T, U><V> (OpenBase<T, U>) -> (U, Public.Type) -> ()
// CHECK-NOT:     #OpenBase.subscript!modify2: #OpenBase.subscript!modify: <T, U><V> (OpenBase<T, U>) -> (U, Public.Type) -> ()
// CHECK-NEXT:  }

// CHECK-NOT: sil_default_override_table PublicBase {

// CHECK-NOT: sil_default_override_table InternalBase {

// CHECK-FRAGILE-NOT: sil_default_override_table OpenBase {
// CHECK-FRAGILE-NOT: sil_default_override_table PublicBase {
// CHECK-FRAGILE-NOT: sil_default_override_table InternalBase {
