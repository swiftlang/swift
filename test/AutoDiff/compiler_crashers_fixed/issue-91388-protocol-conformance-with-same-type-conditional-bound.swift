// RUN: %target-swift-frontend -emit-sil -module-name test %s | %FileCheck %s

import _Differentiation

public protocol P: Differentiable {
  @differentiable(reverse)
  static func foo(_ a: Self) -> Self
}

public struct S<Element: Differentiable>: Differentiable {
  var element: Element
}

extension S: P where Element == Double {
  @differentiable(reverse)
  public static func foo(_ a: Self) -> Self {
    a
  }
}

// CHECK-LABEL: sil_differentiability_witness [serialized] [reverse] [parameters 0] [results 0] @$s4test1SVAASdRszrlE3fooyACySdGAEFZ : $@convention(method) (S<Double>, @thin S<Double>.Type) -> S<Double> {
// CHECK-NEXT:    jvp: @$s4test1SVAASdRszrlE3fooyACySdGAEFZTJfSUpSr : $@convention(method) (S<Double>, @thin S<Double>.Type) -> (S<Double>, @owned @callee_guaranteed (S<Double>.TangentVector) -> S<Double>.TangentVector)
// CHECK-NEXT:    vjp: @$s4test1SVAASdRszrlE3fooyACySdGAEFZTJrSUpSr : $@convention(method) (S<Double>, @thin S<Double>.Type) -> (S<Double>, @owned @callee_guaranteed (S<Double>.TangentVector) -> S<Double>.TangentVector)
// CHECK-NEXT:  }

// CHECK-LABEL: sil shared {{.*}}[thunk] @AD__$s4test1SVySdGAA1PA2aEP3fooyxxFZTW_jvp_SU : $@convention(witness_method: P) (@in_guaranteed S<Double>, @thick S<Double>.Type) -> (@out S<Double>, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <S<Double>.TangentVector, S<Double>.TangentVector>) {
// CHECK:         differentiable_function_extract [jvp]
// CHECK:       } // end sil function 'AD__$s4test1SVySdGAA1PA2aEP3fooyxxFZTW_jvp_SU'

// CHECK-LABEL: sil shared {{.*}}[thunk] @AD__$s4test1SVySdGAA1PA2aEP3fooyxxFZTW_vjp_SU : $@convention(witness_method: P) (@in_guaranteed S<Double>, @thick S<Double>.Type) -> (@out S<Double>, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <S<Double>.TangentVector, S<Double>.TangentVector>) {
// CHECK:         differentiable_function_extract [vjp]
// CHECK:       } // end sil function 'AD__$s4test1SVySdGAA1PA2aEP3fooyxxFZTW_vjp_SU'

// CHECK-LABEL: // reverse-mode derivative of static S<>.foo(_:)
// CHECK:       sil @$s4test1SVAASdRszrlE3fooyACySdGAEFZTJrSUpSr : $@convention(method) (S<Double>, @thin S<Double>.Type) -> (S<Double>, @owned @callee_guaranteed (S<Double>.TangentVector) -> S<Double>.TangentVector) {
// CHECK:         // function_ref pullback of static S<>.foo(_:)
// CHECK:         function_ref @$s4test1SVAASdRszrlE3fooyACySdGAEFZTJpSUpSr : $@convention(thin) (S<Double>.TangentVector) -> S<Double>.TangentVector
// CHECK:       } // end sil function '$s4test1SVAASdRszrlE3fooyACySdGAEFZTJrSUpSr'

// CHECK-LABEL: // pullback of static S<>.foo(_:)
// CHECK:       sil private @$s4test1SVAASdRszrlE3fooyACySdGAEFZTJpSUpSr : $@convention(thin) (S<Double>.TangentVector) -> S<Double>.TangentVector {
// CHECK:       bb0(%0 : $S<Double>.TangentVector):
// CHECK:         return %0
// CHECK:       } // end sil function '$s4test1SVAASdRszrlE3fooyACySdGAEFZTJpSUpSr'

// CHECK-LABEL: sil_witness_table <Element where Element == Double> S<Element>: P module test {
// CHECK:         method #P.foo!jvp.SU.<Self where Self : P>: <Self where Self : P> (Self.Type) -> (Self) -> Self : @AD__$s4test1SVySdGAA1PA2aEP3fooyxxFZTW_jvp_SU
// CHECK:         method #P.foo!vjp.SU.<Self where Self : P>: <Self where Self : P> (Self.Type) -> (Self) -> Self : @AD__$s4test1SVySdGAA1PA2aEP3fooyxxFZTW_vjp_SU
// CHECK:       }
