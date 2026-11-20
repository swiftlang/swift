// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name adcs_function_conversion %s -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %s --check-prefix=INPUT < %t.passes
// RUN: %FileCheck %s --check-prefix=DIRECT < %t.passes
// RUN: %FileCheck %s --check-prefix=GENERIC < %t.passes
// RUN: %FileCheck %s --check-prefix=FINAL < %t.sil
// RUN: %target-swift-frontend -O -g -sil-verify-all -emit-sil -module-name adcs_function_conversion %s | %FileCheck %s --check-prefix=FINAL

// INPUT-LABEL: {{^}}// reverse-mode derivative of branchConverted(_:_:_:)
// INPUT:         %[[#ROOT:]] = partial_apply {{.*}} : $@convention(thin) @Sendable
// INPUT:       debug_value %[[#ROOT]], let, name "callback"
// INPUT:         %[[#CONVERTED:]] = convert_function %[[#ROOT]] to $@callee_guaranteed (Float) -> Float
// INPUT:       debug_value %[[#CONVERTED]], let, name "converted"
// INPUT:         %[[#PAYLOAD:]] = tuple {{.*}}%[[#CONVERTED]]
// INPUT-NEXT:  {{.*}} = enum $_AD__{{.*}}, %[[#PAYLOAD]]

// INPUT-LABEL: {{^}}// reverse-mode derivative of branchSubset(_:_:_:_:)
// INPUT:         %[[#ROOT:]] = partial_apply {{.*}} : $@convention(thin) @Sendable
// INPUT-NEXT:    %[[#CONVERTED:]] = convert_function %[[#ROOT]] to $@callee_guaranteed (Float) -> (Float, Float)
// INPUT:       debug_value %[[#CONVERTED]], let, name "callback"
// INPUT:         // function_ref autodiff subset parameters thunk for pullback
// INPUT-NEXT:    %[[#THUNK:]] = function_ref
// INPUT-NEXT:    %[[#ADAPTED:]] = partial_apply {{.*}} %[[#THUNK]](%[[#CONVERTED]])
// INPUT-NEXT:    %[[#PAYLOAD:]] = tuple {{.*}}%[[#ADAPTED]]
// INPUT-NEXT:  {{.*}} = enum $_AD__{{.*}}, %[[#PAYLOAD]]

// INPUT-LABEL: {{^}}// reverse-mode derivative of branchGeneric<A>(_:_:_:)
// INPUT:         // function_ref closure #1 in vjpGenericTagged<A>(_:_:)
// INPUT-NEXT:    %[[#CALLBACK:]] = function_ref
// INPUT:         %[[#ROOT:]] = partial_apply {{.*}} %[[#CALLBACK]]<τ_0_0>({{.*}}) : $@convention(thin) @Sendable
// INPUT-NEXT:    %[[#CONVERTED:]] = convert_function %[[#ROOT]] to $@callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_0
// INPUT-NEXT:    // function_ref thunk for @escaping @callee_guaranteed (@in_guaranteed A) -> (@out A)
// INPUT-NEXT:    %[[#THUNK:]] = function_ref
// INPUT-NEXT:    %[[#ADAPTED:]] = partial_apply {{.*}} %[[#THUNK]]<τ_0_0>(%[[#CONVERTED]])
// INPUT-NEXT:    %[[#OUTER:]] = convert_function %[[#ADAPTED]] to $@callee_guaranteed @substituted {{.*}} for <τ_0_0, τ_0_0>
// INPUT-NEXT:    %[[#PAYLOAD:]] = tuple {{.*}}%[[#OUTER]]
// INPUT-NEXT:  {{.*}} = enum $_AD__{{.*}}, %[[#PAYLOAD]]

// SILCombine can fold the closure application once ADCS moves it into the pullback.
// DIRECT-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: {{.*}}branchConverted{{.*}}TJp
// DIRECT-NEXT:  // specialized pullback of branchConverted(_:_:_:)
// DIRECT:       // specialized pullback of branchConverted(_:_:_:)
// DIRECT-NOT:   } // end sil function
// DIRECT:         // function_ref closure #1 in vjpTagged(_:_:)
// DIRECT-NEXT:    %[[#CALLBACK:]] = function_ref
// DIRECT-NEXT:    %[[#ROOT:]] = partial_apply {{.*}} %[[#CALLBACK]]({{.*}}) : $@convention(thin) @Sendable
// DIRECT-NEXT:    %[[#CONVERTED:]] = convert_function %[[#ROOT]] to $@callee_guaranteed (Float) -> Float
// DIRECT-NEXT:  {{.*}} = apply %[[#CONVERTED]]({{.*}}) : $@callee_guaranteed (Float) -> Float
// DIRECT:       } // end sil function

// GENERIC-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: {{.*}}branchGeneric{{.*}}TJp
// GENERIC-NEXT:  // specialized pullback of branchGeneric<A>(_:_:_:)
// GENERIC:       // specialized pullback of branchGeneric<A>(_:_:_:)
// GENERIC-NOT:   } // end sil function
// GENERIC:         // function_ref closure #1 in vjpGenericTagged<A>(_:_:)
// GENERIC-NEXT:    %[[#CALLBACK:]] = function_ref
// GENERIC-NEXT:    %[[#ROOT:]] = partial_apply {{.*}} %[[#CALLBACK]]<τ_0_0>({{.*}}) : $@convention(thin) @Sendable
// GENERIC-NEXT:    %[[#CONVERTED:]] = convert_function %[[#ROOT]] to $@callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_0
// GENERIC-NEXT:    // function_ref thunk for @escaping @callee_guaranteed (@in_guaranteed A) -> (@out A)
// GENERIC-NEXT:    %[[#THUNK:]] = function_ref
// GENERIC-NEXT:    %[[#ADAPTED:]] = partial_apply {{.*}} %[[#THUNK]]<τ_0_0>(%[[#CONVERTED]])
// GENERIC-NEXT:    %[[#OUTER:]] = convert_function %[[#ADAPTED]] to $@callee_guaranteed @substituted {{.*}} for <τ_0_0, τ_0_0>
// GENERIC-NEXT:  {{.*}} = apply %[[#OUTER]]({{.*}}) : $@callee_guaranteed @substituted {{.*}} for <τ_0_0, τ_0_0>
// GENERIC:       } // end sil function

// FINAL-LABEL: {{^}}// reverse-mode derivative of branchConverted(_:_:_:)
// FINAL-NOT:     = partial_apply
// FINAL-NOT:     = convert_function
// FINAL:         // function_ref specialized pullback of branchConverted(_:_:_:)
// FINAL-NEXT:    %[[#FN:]] = function_ref
// FINAL-NEXT:    %[[#PB:]] = partial_apply {{.*}} %[[#FN]]
// FINAL-NEXT:    %[[#RESULT:]] = tuple (%[[#]], %[[#PB]])
// FINAL-NEXT:    return %[[#RESULT]]
// FINAL:       } // end sil function
// FINAL-LABEL: {{^}}// specialized pullback of branchConverted(_:_:_:)
// FINAL-NOT:     = partial_apply
// FINAL-NOT:     apply {{.*}} : $@callee_guaranteed
// FINAL:       } // end sil function

// FINAL-LABEL: {{^}}// reverse-mode derivative of branchSubset(_:_:_:_:)
// FINAL-NOT:     = partial_apply
// FINAL-NOT:     = convert_function
// FINAL:         // function_ref specialized pullback of branchSubset(_:_:_:_:)
// FINAL-NEXT:    %[[#FN:]] = function_ref
// FINAL-NEXT:    %[[#PB:]] = partial_apply {{.*}} %[[#FN]]
// FINAL-NEXT:    %[[#RESULT:]] = tuple (%[[#]], %[[#PB]])
// FINAL-NEXT:    return %[[#RESULT]]
// FINAL:       } // end sil function
// FINAL-LABEL: {{^}}// specialized pullback of branchSubset(_:_:_:_:)
// FINAL-NOT:     = partial_apply
// FINAL-NOT:     apply {{.*}} : $@callee_guaranteed
// FINAL:       } // end sil function

import _Differentiation

@differentiable(reverse, wrt: x)
@inline(never)
public func tagged(_ x: Float, _ label: String) -> Float { x * x }

// Keep debug uses of both the @Sendable closure and its converted value.
@derivative(of: tagged, wrt: x)
@inline(__always)
public func vjpTagged(_ x: Float, _ label: String)
  -> (value: Float, pullback: (Float) -> Float) {
  let callback: @Sendable (Float) -> Float = { seed in
    withExtendedLifetime(label) { 2 * x * seed }
  }
  let converted: (Float) -> Float = callback
  return (x * x, converted)
}

@differentiable(reverse, wrt: x)
public func branchConverted(_ x: Float, _ label: String, _ flag: Bool) -> Float {
  if flag { return tagged(x, label) }
  return x + x
}

@differentiable(reverse, wrt: (x, y))
@inline(never)
public func taggedPair(_ x: Float, _ y: Float, _ label: String) -> Float {
  x * x + y * y
}

@derivative(of: taggedPair, wrt: (x, y))
@inline(__always)
public func vjpTaggedPair(_ x: Float, _ y: Float, _ label: String)
  -> (value: Float, pullback: (Float) -> (Float, Float)) {
  // Here the debug use belongs to the converted callback.
  let callback: (Float) -> (Float, Float) = { @Sendable seed in
    withExtendedLifetime(label) { (2 * x * seed, 2 * y * seed) }
  }
  return (x * x + y * y, callback)
}

@differentiable(reverse, wrt: x)
public func branchSubset(_ x: Float, _ y: Float, _ label: String, _ flag: Bool) -> Float {
  if flag { return taggedPair(x, y, label) }
  return x + x
}

@differentiable(reverse, wrt: x)
@inline(never)
public func genericTagged<T: BinaryFloatingPoint & Differentiable & Sendable>(
  _ x: T, _ label: String
) -> T where T.TangentVector == T {
  x + x
}

@derivative(of: genericTagged, wrt: x)
@inline(__always)
public func vjpGenericTagged<T: BinaryFloatingPoint & Differentiable & Sendable>(
  _ x: T, _ label: String
) -> (value: T, pullback: (T) -> T) where T.TangentVector == T {
  (x + x, { @Sendable seed in
    withExtendedLifetime(label) { seed + seed }
  })
}

@differentiable(reverse, wrt: x)
public func branchGeneric<T: BinaryFloatingPoint & Differentiable & Sendable>(
  _ x: T, _ label: String, _ flag: Bool
) -> T where T.TangentVector == T {
  if flag { return genericTagged(x, label) }
  return x + x
}
