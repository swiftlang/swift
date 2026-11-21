// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-Onone)
// RUN: %target-run-simple-swift(-O)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name adcs_function_conversion %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation

@differentiable(reverse, wrt: x)
@inline(never)
public func tagged(_ x: Float, _ label: String) -> Float { x * x }

// Naming both values gives the original and converted closures separate debug uses.
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
  // Name only the converted closure to give it the debug use.
  let callback: (Float) -> (Float, Float) = { @Sendable seed in
    withExtendedLifetime(label) { (2 * x * seed, 2 * y * seed) }
  }
  return (x * x + y * y, callback)
}

// The subset-parameters thunk captures the converted callback.
@differentiable(reverse, wrt: x)
public func branchSubset(_ x: Float, _ y: Float, _ label: String, _ flag: Bool) -> Float {
  if flag { return taggedPair(x, y, label) }
  return x + x
}

// An address-only callback also needs a generic reabstraction thunk.
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

// CHECK-LABEL: {{^}}// reverse-mode derivative of branchConverted(_:_:_:)
// CHECK:         %[[#DIRECT_PAYLOAD:]] = tuple $(predecessor: {{[^,]+}}, (String, Float))
// CHECK-NEXT:    %[[#]] = enum $[[DIRECT_TAPE:_AD__[^ ,]+_spec[^ ,]+]], #[[DIRECT_TAPE]].bb1!enumelt, %[[#DIRECT_PAYLOAD]]
// CHECK:         // function_ref specialized pullback of branchConverted(_:_:_:)
// CHECK-NEXT:    %[[#DIRECT_PB:]] = function_ref @[[DIRECT_PULLBACK:[^ ]+]]
// CHECK-NEXT:    %[[#]] = partial_apply [callee_guaranteed] %[[#DIRECT_PB]](%[[#]])
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// specialized pullback of branchConverted(_:_:_:)
// CHECK:       sil private @[[DIRECT_PULLBACK]]
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// reverse-mode derivative of branchSubset(_:_:_:_:)
// CHECK:         %[[#SUBSET_PAYLOAD:]] = tuple $(predecessor: {{[^,]+}}, (String, Float, Float))
// CHECK-NEXT:    %[[#]] = enum $[[SUBSET_TAPE:_AD__[^ ,]+_spec[^ ,]+]], #[[SUBSET_TAPE]].bb1!enumelt, %[[#SUBSET_PAYLOAD]]
// CHECK:         // function_ref specialized pullback of branchSubset(_:_:_:_:)
// CHECK-NEXT:    %[[#SUBSET_PB:]] = function_ref @[[SUBSET_PULLBACK:[^ ]+]]
// CHECK-NEXT:    %[[#]] = partial_apply [callee_guaranteed] %[[#SUBSET_PB]](%[[#]])
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// specialized pullback of branchSubset(_:_:_:_:)
// CHECK:       sil private @[[SUBSET_PULLBACK]]
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// reverse-mode derivative of branchGeneric<A>(_:_:_:)
// CHECK:         %[[#GEN_PAYLOAD:]] = tuple $(predecessor: {{[^,]+}}<τ_0_0>, (_: String))
// CHECK-NEXT:    %[[#]] = enum $[[GEN_TAPE:_AD__[^ ,]+_spec[^ ,<]+]]<τ_0_0>, #[[GEN_TAPE]].bb1!enumelt, %[[#GEN_PAYLOAD]]
// CHECK:         // function_ref specialized pullback of branchGeneric<A>(_:_:_:)
// CHECK-NEXT:    %[[#GEN_PB:]] = function_ref @[[GEN_PULLBACK:[^ ]+]]
// CHECK-NEXT:    %[[#]] = partial_apply [callee_guaranteed] %[[#GEN_PB]]<τ_0_0>(%[[#]])
// CHECK:       } // end sil function

// CHECK:       sil private @[[GEN_PULLBACK]]
// CHECK:         %[[#CAPTURES:]] = tuple_extract %[[#]], 1
// CHECK:         %[[#LABEL:]] = tuple_extract %[[#CAPTURES]], 0
// CHECK:         // function_ref closure #1 in vjpGenericTagged<A>(_:_:)
// CHECK-NEXT:    %[[#ROOT:]] = function_ref
// CHECK-NEXT:    %[[#CALLBACK:]] = partial_apply [callee_guaranteed] %[[#ROOT]]<τ_0_0>(%[[#LABEL]]) : $@convention(thin) @Sendable
// CHECK-NEXT:    %[[#CONVERTED:]] = convert_function %[[#CALLBACK]] to $@callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK-NEXT:    %[[#]] = apply %[[#CONVERTED]]({{.*}}) : $@callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:       } // end sil function

let label = String(repeating: "a captured label ", count: 5)
for flag in [false, true] {
  for x: Float in [-3, 0, 3] {
    let expectedDerivative: Float = flag ? 2 * x : 2
    let direct = valueWithPullback(at: x, of: { branchConverted($0, label, flag) })
    precondition(direct.value == (flag ? x * x : 2 * x))
    precondition(direct.pullback(1) == expectedDerivative)
    precondition(direct.pullback(3) == 3 * expectedDerivative)

    let subset = valueWithPullback(at: x, of: { branchSubset($0, 4, label, flag) })
    precondition(subset.value == (flag ? x * x + 16 : 2 * x))
    precondition(subset.pullback(1) == expectedDerivative)
    precondition(subset.pullback(3) == 3 * expectedDerivative)

    let genericFloat = valueWithPullback(at: x, of: { branchGeneric($0, label, flag) })
    precondition(genericFloat.value == 2 * x)
    precondition(genericFloat.pullback(1) == 2)
    precondition(genericFloat.pullback(3) == 6)

    let genericDouble = valueWithPullback(at: Double(x), of: { branchGeneric($0, label, flag) })
    precondition(genericDouble.value == 2 * Double(x))
    precondition(genericDouble.pullback(1) == 2)
    precondition(genericDouble.pullback(3) == 6)
  }
}
