// RUN: %target-swift-frontend -module-name constant_propagation_stdlib %s -Xllvm -sil-print-types -emit-sil -enable-builtin-module -o -    | %FileCheck --check-prefix=CHECK-ONONE %s
// RUN: %target-swift-frontend -module-name constant_propagation_stdlib %s -Xllvm -sil-print-types -emit-sil -enable-builtin-module -o - -O | %FileCheck --check-prefix=CHECK-O %s

// REQUIRES: swift_in_compiler

import Builtin

public struct MyInt {
  var v: Builtin.Int32
}

// CHECK-ONONE-LABEL: sil @$s27constant_propagation_stdlib15isConcrete_trueyBi1_AA5MyIntVF : $@convention(thin) (MyInt) -> Builtin.Int1 {
// CHECK-ONONE:       bb0(
// CHECK-ONONE:         [[RESULT:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK-ONONE:         return [[RESULT]]
// CHECK-ONONE:       } // end sil function '$s27constant_propagation_stdlib15isConcrete_trueyBi1_AA5MyIntVF'
// CHECK-O-LABEL:     sil @$s27constant_propagation_stdlib15isConcrete_trueyBi1_AA5MyIntVF : $@convention(thin) (MyInt) -> Builtin.Int1 {
// CHECK-O:           bb0(
// CHECK-O:             [[RESULT:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK-O-NEXT:        return [[RESULT]]
// CHECK-O-NEXT:      } // end sil function '$s27constant_propagation_stdlib15isConcrete_trueyBi1_AA5MyIntVF'
public func isConcrete_true(_ x: MyInt) -> Builtin.Int1 {
  return Builtin.isConcrete(MyInt.self)
}

// CHECK-ONONE-LABEL: sil @$s27constant_propagation_stdlib16isConcrete_falseyBi1_xlF : $@convention(thin) <T> (@in_guaranteed T) -> Builtin.Int1 {
// CHECK-ONONE:       bb0(
// CHECK-ONONE:         [[RESULT:%.*]] = integer_literal $Builtin.Int1, 0
// CHECK-ONONE:         return [[RESULT]]
// CHECK-ONONE:       } // end sil function '$s27constant_propagation_stdlib16isConcrete_falseyBi1_xlF'
// CHECK-O-LABEL:     sil [signature_optimized_thunk] [always_inline] {{.*}}@$s27constant_propagation_stdlib16isConcrete_falseyBi1_xlF : $@convention(thin) <T> (@in_guaranteed T) -> Builtin.Int1 {
// CHECK-O:           bb0(
// CHECK-O:             [[GEN_FUNC:%.*]] = function_ref @$s27constant_propagation_stdlib16isConcrete_falseyBi1_xlFTf4d_n : $@convention(thin) <τ_0_0> () -> Builtin.Int1
// CHECK-O:             [[RESULT:%.*]] = apply [[GEN_FUNC]]<T>() : $@convention(thin) <τ_0_0> () -> Builtin.Int1
// CHECK-O:             return [[RESULT]]
// CHECK-O:           } // end sil function '$s27constant_propagation_stdlib16isConcrete_falseyBi1_xlF'
public func isConcrete_false<T>(_ x: T) -> Builtin.Int1 {
  return Builtin.isConcrete(T.self)
}

// CHECK-ONONE-LABEL: sil @$s27constant_propagation_stdlib25isConcrete_generic_calleryBi1_xlF : $@convention(thin) <T> (@in_guaranteed T) -> Builtin.Int1 {
// CHECK-ONONE:       bb0(
// CHECK-ONONE:         [[GEN_FUNC:%.*]] = function_ref @$s27constant_propagation_stdlib16isConcrete_falseyBi1_xlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> Builtin.Int1
// CHECK-ONONE:         [[RESULT:%.*]] = apply [[GEN_FUNC]]<T>(%0) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> Builtin.Int1
// CHECK-ONONE:         return [[RESULT]]
// CHECK-ONONE:       } // end sil function '$s27constant_propagation_stdlib25isConcrete_generic_calleryBi1_xlF'
// CHECK-O-LABEL:     sil {{.*}}@$s27constant_propagation_stdlib25isConcrete_generic_calleryBi1_xlF : $@convention(thin) <T> (@in_guaranteed T) -> Builtin.Int1 {
// CHECK-O:           bb0(
// CHECK-O:             [[GEN_FUNC:%.*]] = function_ref @$s27constant_propagation_stdlib16isConcrete_falseyBi1_xlFTf4d_n : $@convention(thin) <τ_0_0> () -> Builtin.Int1
// CHECK-O:             [[RESULT:%.*]] = apply [[GEN_FUNC]]<T>() : $@convention(thin) <τ_0_0> () -> Builtin.Int1
// CHECK-O:             return [[RESULT]]
// CHECK-O:           } // end sil function '$s27constant_propagation_stdlib25isConcrete_generic_calleryBi1_xlF'
public func isConcrete_generic_caller<T>(_ x: T) -> Builtin.Int1 {
  return isConcrete_false(x)  
}

// CHECK-ONONE-LABEL: sil @$s27constant_propagation_stdlib26isConcrete_concrete_calleryBi1_AA5MyIntVF : $@convention(thin) (MyInt) -> Builtin.Int1 {
// CHECK-ONONE:       bb0(
// CHECK-ONONE:         [[STACK_ARG:%.*]] = alloc_stack $MyInt
// CHECK-ONONE:         store %0 to [[STACK_ARG]] : $*MyInt
// CHECK-ONONE:         [[GEN_FUNC:%.*]] = function_ref @$s27constant_propagation_stdlib25isConcrete_generic_calleryBi1_xlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> Builtin.Int1
// CHECK-ONONE:         [[RESULT:%.*]] = apply [[GEN_FUNC]]<MyInt>([[STACK_ARG]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> Builtin.Int1
// CHECK-ONONE:         dealloc_stack [[STACK_ARG]] : $*MyInt
// CHECK-ONONE:         return [[RESULT]]
// CHECK-ONONE:       } // end sil function '$s27constant_propagation_stdlib26isConcrete_concrete_calleryBi1_AA5MyIntVF'
// CHECK-O-LABEL:     sil @$s27constant_propagation_stdlib26isConcrete_concrete_calleryBi1_AA5MyIntVF : $@convention(thin) (MyInt) -> Builtin.Int1 {
// CHECK-O:           bb0(
// CHECK-O:             [[RESULT:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK-O:             return [[RESULT]]
// CHECK-O:           } // end sil function '$s27constant_propagation_stdlib26isConcrete_concrete_calleryBi1_AA5MyIntVF'
public func isConcrete_concrete_caller(_ x: MyInt) -> Builtin.Int1 {
  return isConcrete_generic_caller(x)
}

// CHECK-ONONE-LABEL: sil @$s27constant_propagation_stdlib4main1xBi1__Bi1_Bi1_tAA5MyIntV_tF : $@convention(thin) (MyInt) -> (Builtin.Int1, Builtin.Int1, Builtin.Int1) {
// CHECK-ONONE:       bb0(
// CHECK-ONONE:         [[IS_CONCRETE_TRUE_FUNC:%.*]] = function_ref @$s27constant_propagation_stdlib15isConcrete_trueyBi1_AA5MyIntVF : $@convention(thin) (MyInt) -> Builtin.Int1
// CHECK-ONONE:         [[IS_CONCRETE_TRUE:%.*]] = apply [[IS_CONCRETE_TRUE_FUNC]](%0) : $@convention(thin) (MyInt) -> Builtin.Int1
// CHECK-ONONE:         [[STACK_ARG:%.*]] = alloc_stack $MyInt
// CHECK-ONONE:         store %0 to [[STACK_ARG]] : $*MyInt
// CHECK-ONONE:         [[IS_CONCRETE_FALSE_FUNC:%.*]] = function_ref @$s27constant_propagation_stdlib16isConcrete_falseyBi1_xlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> Builtin.Int1
// CHECK-ONONE:         [[IS_CONCRETE_FALSE:%.*]] = apply [[IS_CONCRETE_FALSE_FUNC]]<MyInt>([[STACK_ARG]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> Builtin.Int1
// CHECK-ONONE:         dealloc_stack [[STACK_ARG]] : $*MyInt
// CHECK-ONONE:         [[IS_CONCRETE_CONCRETE_CALLER_FUNC:%.*]] = function_ref @$s27constant_propagation_stdlib26isConcrete_concrete_calleryBi1_AA5MyIntVF : $@convention(thin) (MyInt) -> Builtin.Int1
// CHECK-ONONE:         [[IS_CONCRETE_CONCRETE_CALLER:%.*]] = apply [[IS_CONCRETE_CONCRETE_CALLER_FUNC]](%0) : $@convention(thin) (MyInt) -> Builtin.Int1
// CHECK-ONONE:         [[RESULT:%.*]] = tuple ([[IS_CONCRETE_TRUE]] : $Builtin.Int1, [[IS_CONCRETE_FALSE]] : $Builtin.Int1, [[IS_CONCRETE_CONCRETE_CALLER]] : $Builtin.Int1)
// CHECK-ONONE:         return [[RESULT]]
// CHECK-ONONE:       } // end sil function '$s27constant_propagation_stdlib4main1xBi1__Bi1_Bi1_tAA5MyIntV_tF'
// CHECK-O-LABEL:     sil @$s27constant_propagation_stdlib4main1xBi1__Bi1_Bi1_tAA5MyIntV_tF : $@convention(thin) (MyInt) -> (Builtin.Int1, Builtin.Int1, Builtin.Int1) {
// CHECK-O:           bb0(
// CHECK-O:             [[VALUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK-O:             [[RESULT:%.*]] = tuple ([[VALUE]] : $Builtin.Int1, [[VALUE]] : $Builtin.Int1, [[VALUE]] : $Builtin.Int1)
// CHECK-O-NEXT:        return [[RESULT]]
// CHECK-O-NEXT:      } // end sil function '$s27constant_propagation_stdlib4main1xBi1__Bi1_Bi1_tAA5MyIntV_tF'
public func main(x: MyInt) -> (Builtin.Int1, Builtin.Int1, Builtin.Int1) {
  return (isConcrete_true(x), isConcrete_false(x), isConcrete_concrete_caller(x))
}
