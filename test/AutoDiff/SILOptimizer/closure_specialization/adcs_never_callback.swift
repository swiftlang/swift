// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name adcs_never_callback %s -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s --check-prefix=BEFORE < %t.passes
// RUN: %FileCheck %s --check-prefix=AFTER < %t.passes

// BEFORE-LABEL: {{^}}// reverse-mode derivative of branchGenericNever(_:_:)
// BEFORE:         // function_ref specialized closure #1 in genericPullback<A>(_:_:)
// BEFORE-NEXT:    %[[#ROOT_FN:]] = function_ref
// BEFORE-NEXT:    %[[#ROOT:]] = partial_apply {{.*}} %[[#ROOT_FN]](%[[#]]) : {{.*}} (Float, Float) -> (Float, @error_indirect Never)
// BEFORE:         // function_ref thunk for @escaping
// BEFORE-NEXT:    %[[#THUNK_FN:]] = function_ref {{.*}} : {{.*}} (Float, @guaranteed @callee_guaranteed (Float) -> (Float, @error_indirect Never)) -> Float
// BEFORE-NEXT:    %[[#CALLBACK:]] = partial_apply {{.*}} %[[#THUNK_FN]](%[[#ROOT]])
// BEFORE-NEXT:    %[[#PAYLOAD:]] = tuple {{.*}}@callee_guaranteed (Float) -> Float) ({{.*}}%[[#CALLBACK]])
// BEFORE-NEXT:  {{.*}} = enum {{.*}}, %[[#PAYLOAD]]

// AFTER-LABEL: *** New SIL function in {{.*}}stage HighLevel,Module+StackPromote, pass: AutodiffClosureSpecialization {{.*}}Function: {{.*}}18branchGenericNever{{.*}}TJp
// AFTER-NEXT:  // specialized pullback of branchGenericNever(_:_:)
// AFTER:       // specialized pullback of branchGenericNever(_:_:)
// AFTER:       bb[[#]](%[[#PAYLOAD:]] : $(predecessor: {{.*}}, (_: Float))):
// AFTER-NEXT:    (%[[#]], %[[#CAPTURES:]]) = destructure_tuple %[[#PAYLOAD]]
// AFTER-NEXT:    %[[#CAPTURE:]] = tuple_extract %[[#CAPTURES]], 0
// AFTER:         // function_ref specialized closure #1 in genericPullback<A>(_:_:)
// AFTER-NEXT:    %[[#ROOT_FN:]] = function_ref
// AFTER-NEXT:    %[[#ROOT:]] = partial_apply {{.*}} %[[#ROOT_FN]](%[[#CAPTURE]]) : {{.*}} (Float, Float) -> (Float, @error_indirect Never)
// AFTER:         // function_ref thunk for @escaping
// AFTER-NEXT:    %[[#THUNK_FN:]] = function_ref {{.*}} : {{.*}} (Float, @guaranteed @callee_guaranteed (Float) -> (Float, @error_indirect Never)) -> Float
// AFTER-NEXT:  {{.*}} = apply %[[#THUNK_FN]](%[[#]], %[[#ROOT]]) : {{.*}} -> Float
// AFTER:         destroy_value %[[#ROOT]]
// AFTER:       } // end sil function

import _Differentiation

// Instantiating the error type with Never leaves an indirect error result.
// A nonthrowing reabstraction thunk adapts the root to the pullback type.
@inline(__always)
public func genericPullback<E: Error>(_ x: Float, _: E.Type)
  -> (Float) throws(E) -> Float {
  { (seed: Float) throws(E) -> Float in 2 * x * seed }
}

@differentiable(reverse)
public func genericNeverSquare(_ x: Float) -> Float { x * x }

@derivative(of: genericNeverSquare)
@inline(__always)
public func vjpGenericNeverSquare(_ x: Float)
  -> (value: Float, pullback: (Float) -> Float) {
  (x * x, genericPullback(x, Never.self))
}

@differentiable(reverse, wrt: x)
public func branchGenericNever(_ x: Float, _ flag: Bool) -> Float {
  if flag { return genericNeverSquare(x) }
  return x + x
}
