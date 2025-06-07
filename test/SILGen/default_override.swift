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

open class OpenBase<T> {
  open var openField: T {
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseC9openFieldxvyTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0> 
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @in_guaranteed τ_0_0
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.openField!read
// CHECK-SAME:        <T> (OpenBase<T>) -> () -> ()
// CHECK-SAME:        $@yield_once @convention(method) <τ_0_0> (@guaranteed OpenBase<τ_0_0>) -> @yields @in_guaranteed τ_0_0
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0>([[SELF]])
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
// CHECK-SAME:         <τ_0_0>
// CHECK-SAME:         (
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0>
// CHECK-SAME:         )
// CHECK-SAME:             ->
// CHECK-SAME:         @yields @inout τ_0_0
// CHECK-SAME:  {
// CHECK:       {{bb[0-9]+}}(
// CHECK-SAME:      [[SELF:%[^,]+]] :
// CHECK-SAME:  ):
// CHECK:         [[ORIGINAL:%[^,]+]] = class_method [[SELF]], #OpenBase.openField!modify
// CHECK:             <T> (OpenBase<T>) -> () -> ()
// CHECK:             $@yield_once @convention(method) <τ_0_0> (@guaranteed OpenBase<τ_0_0>) -> @yields @inout τ_0_0
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0>([[SELF]])
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
  open subscript<U>(_ u: U, _ _: Open.Type) -> T {
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseCyxqd___AA0C0OmtcluiyTwd 
// CHECK-SAME:      : $@yield_once_2 
// CHECK-SAME:         @convention(method) 
// CHECK-SAME:         <τ_0_0><τ_1_0> 
// CHECK-SAME:         (
// CHECK-SAME:             @in_guaranteed τ_1_0
// CHECK-SAME:             @thin Open.Type
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0>
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
// CHECK:             #OpenBase.subscript!read
// CHECK:             <T><U> (OpenBase<T>) -> (U, Open.Type) -> ()
// CHECK:             $@yield_once @convention(method) <τ_0_0><τ_1_0> (@in_guaranteed τ_1_0, @thin Open.Type, @guaranteed OpenBase<τ_0_0>) -> @yields @in_guaranteed τ_0_0
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_1_0>([[KEY]], [[OPEN_TY]], [[SELF]])
// CHECK:         yield [[ADDR]]
// CHECK:             resume bb1
// CHECK:             unwind bb2
// CHECK:       bb1:
// CHECK:         end_apply [[TOKEN]]
// CHECK:         [[RETVAL:%[^,]+]] = tuple ()
// CHECK:         return [[RETVAL]]
// CHECK:       bb2:
// CHECK:         abort_apply [[TOKEN]]
// CHECK:         unwind
// CHECK-LABEL: } // end sil function '$s16default_override8OpenBaseCyxqd___AA0C0OmtcluiyTwd'
    read {
      Builtin.int_trap()
    }
// CHECK-LABEL: sil shared [serialized] [ossa] @$s16default_override8OpenBaseCyxqd___AA0C0OmtcluixTwd
// CHECK-SAME:      : $@yield_once_2
// CHECK-SAME:         @convention(method)
// CHECK-SAME:         <τ_0_0><τ_1_0>
// CHECK-SAME:         (
// CHECK-SAME:             @in_guaranteed τ_1_0
// CHECK-SAME:             @thin Open.Type
// CHECK-SAME:             @guaranteed OpenBase<τ_0_0>
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
// CHECK-SAME:        <T><U> (OpenBase<T>) -> (U, Open.Type) -> ()
// CHECK-SAME:        $@yield_once @convention(method) <τ_0_0><τ_1_0> (@in_guaranteed τ_1_0, @thin Open.Type, @guaranteed OpenBase<τ_0_0>) -> @yields @inout τ_0_0
// CHECK:         ([[ADDR:%[^,]+]], [[TOKEN:%[^,]+]]) = begin_apply [[ORIGINAL]]<τ_0_0, τ_1_0>([[KEY]], [[OPEN_TY]], [[SELF]])
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
  public subscript<U>(_ u: U, _ _: Public.Type) -> T {
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
  subscript<U>(_ u: U, _ _: Internal.Type) -> T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
}

public class PublicBase<T> {
  open var openField: T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  open subscript<U>(_ u: U, _ _: Open.Type) -> T {
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
  public subscript<U>(_ u: U, _ _: Public.Type) -> T {
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
  subscript<U>(_ u: U, _ _: Internal.Type) -> T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
}

class InternalBase<T> {
  open var openField: T {
    read {
      Builtin.int_trap()
    }
    modify {
      Builtin.int_trap()
    }
  }
  open subscript<U>(_ u: U, _ _: Open.Type) -> T {
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
  public subscript<U>(_ u: U, _ _: Public.Type) -> T {
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
  subscript<U>(_ u: U, _ _: Internal.Type) -> T {
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
// CHECK-SAME:        <T> (OpenBase<T>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC9openFieldxvyTwd
// CHECK-NEXT:    #OpenBase.openField!modify2
// CHECK-SAME:        #OpenBase.openField!modify
// CHECK-SAME:        <T> (OpenBase<T>) -> () -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseC9openFieldxvxTwd
// CHECK-NEXT:    #OpenBase.subscript!read2
// CHECK-SAME:        #OpenBase.subscript!read
// CHECK-SAME:        <T><U> (OpenBase<T>) -> (U, Open.Type) -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseCyxqd___AA0C0OmtcluiyTwd
// CHECK-NEXT:    #OpenBase.subscript!modify2
// CHECK-SAME:        #OpenBase.subscript!modify
// CHECK-SAME:        <T><U> (OpenBase<T>) -> (U, Open.Type) -> ()
// CHECK-SAME:        @$s16default_override8OpenBaseCyxqd___AA0C0OmtcluixTwd
// CHECK-NOT:     #OpenBase.publicField!read2
// CHECK-NOT:     #OpenBase.publicField!modify2
// CHECK-NOT:     #OpenBase.subscript!read2: #OpenBase.subscript!read: <T><U> (OpenBase<T>) -> (U, Public.Type) -> ()
// CHECK-NOT:     #OpenBase.subscript!modify2: #OpenBase.subscript!modify: <T><U> (OpenBase<T>) -> (U, Public.Type) -> ()
// CHECK-NEXT:  }

// CHECK-NOT: sil_default_override_table PublicBase {

// CHECK-NOT: sil_default_override_table InternalBase {

// CHECK-FRAGILE-NOT: sil_default_override_table OpenBase {
// CHECK-FRAGILE-NOT: sil_default_override_table PublicBase {
// CHECK-FRAGILE-NOT: sil_default_override_table InternalBase {
