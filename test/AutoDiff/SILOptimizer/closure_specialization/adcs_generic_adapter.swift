// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name adcs_generic_adapter %s -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s < %t.passes
// RUN: %FileCheck %s --check-prefix=THIN < %t.passes
// RUN: %FileCheck %s --check-prefix=FINAL < %t.sil

// CHECK:       sil {{.*}}[reabstraction_thunk] {{.*}} @[[REABSTRACTION:[^ ]+]] : {{.*}} (Float, @guaranteed @callee_guaranteed (@in_guaranteed Float) ->
// CHECK-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: {{[^ ]*}}6Subset{{[^ ]*}}TJp
// CHECK-NEXT:  // specialized pullback of genericSubset<A>(_:_:_:)
// CHECK:       // specialized pullback of genericSubset<A>(_:_:_:)
// CHECK-NOT:   } // end sil function
// CHECK:         // function_ref closure #1 in makePullback<A, B>(_:_:)
// CHECK-NEXT:    %[[#ROOT:]] = function_ref
// CHECK-NEXT:    %[[#INNER:]] = partial_apply {{.*}} %[[#ROOT]]<Float, τ_0_0>()
// CHECK-NEXT:    // function_ref thunk for @escaping
// CHECK-NEXT:    %[[#THUNK:]] = function_ref @[[REABSTRACTION]]
// CHECK-NEXT:    %[[#ADAPTED:]] = partial_apply {{.*}} %[[#THUNK]]<τ_0_0>(%[[#INNER]])
// CHECK-NEXT:    // function_ref autodiff subset parameters thunk for pullback
// CHECK-NEXT:    %[[#SUBSET:]] = function_ref
// CHECK-NEXT:  {{.*}} = apply %[[#SUBSET]]<τ_0_0>(%[[#]], %[[#ADAPTED]])
// CHECK:         destroy_value %[[#ADAPTED]]
// CHECK:       } // end sil function

// THIN-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: {{[^ ]*}}10thinSubset{{[^ ]*}}TJp
// THIN-NEXT:  // specialized pullback of thinSubset(_:_:_:)
// THIN:       // specialized pullback of thinSubset(_:_:_:)
// THIN-NOT:   } // end sil function
// THIN:         %[[#ADD:]] = function_ref @{{.*}}_vjpAdd
// THIN-NEXT:    %[[#THICK:]] = thin_to_thick_function %[[#ADD]]
// THIN-NEXT:    // function_ref autodiff subset parameters thunk for pullback
// THIN-NEXT:    %[[#SUBSET:]] = function_ref
// THIN-NEXT:  {{.*}} = apply %[[#SUBSET]](%[[#]], %[[#THICK]])

// Fixing the generic seed to Float requires reabstraction. Differentiating
// only x adds an outer subset thunk.

import _Differentiation

@inline(__always)
func makePullback<Seed: BinaryFloatingPoint, T: AdditiveArithmetic>(
  _: T.Type, _: Seed.Type
) -> (Seed) -> (Float, T) {
  { seed in (2 * Float(seed), T.zero) }
}

@differentiable(reverse, wrt: (x, y))
@inline(never)
public func mixed<T: Differentiable & AdditiveArithmetic>(_ x: Float, _ y: T) -> Float
where T.TangentVector == T { 2 * x }

@derivative(of: mixed, wrt: (x, y))
@inline(__always)
public func vjpMixed<T: Differentiable & AdditiveArithmetic>(_ x: Float, _ y: T)
  -> (value: Float, pullback: (Float) -> (Float, T)) where T.TangentVector == T {
  (2 * x, makePullback(T.self, Float.self))
}

@differentiable(reverse, wrt: x)
public func genericSubset<T: Differentiable & AdditiveArithmetic>(_ x: Float, _ y: T, _ flag: Bool) -> Float
where T.TangentVector == T {
  if flag { return mixed(x, y) }
  return x * x
}

// Differentiating only x adds a subset thunk around a captureless pullback.
@differentiable(reverse, wrt: x)
public func thinSubset(_ x: Float, _ y: Float, _ flag: Bool) -> Float {
  if flag { return x + y }
  return x * x
}

// FINAL-LABEL: {{^}}// reverse-mode derivative of genericSubset<A>
// FINAL-NOT:   } // end sil function
// FINAL:         // function_ref specialized pullback of genericSubset<A>
// FINAL-NEXT:    %[[#PB:]] = function_ref
// FINAL-NEXT:    %[[#PULLBACK:]] = partial_apply {{.*}} %[[#PB]]{{[<(]}}
// FINAL-NEXT:    %[[#RESULT:]] = tuple (%[[#]], %[[#PULLBACK]])
// FINAL-NOT:   } // end sil function
// FINAL:         return %[[#RESULT]]
// FINAL-NEXT:  } // end sil function

// FINAL-LABEL: {{^}}// specialized pullback of genericSubset<A>
// FINAL-NOT:     = partial_apply
// FINAL-NOT:     apply {{.*}} : $@callee_guaranteed
// FINAL:       } // end sil function
