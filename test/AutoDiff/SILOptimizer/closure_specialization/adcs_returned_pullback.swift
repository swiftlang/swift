// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name ReturnedPullback %s -o %t.sil
// RUN: %FileCheck %s --check-prefix=FINAL < %t.sil
// RUN: %FileCheck %s --check-prefix=CONVERTED < %t.sil

import _Differentiation

@differentiable(reverse)
func fourthPower(_ x: inout Float) {
  x *= x
  x *= x
}

@differentiable(reverse, wrt: x)
public func branch(_ x: inout Float, _ square: Bool) {
  if square { x *= x }
  fourthPower(&x)
}

// FINAL-LABEL: {{^}}// reverse-mode derivative of branch(_:_:)
// FINAL-NOT:     partial_apply
// FINAL:         // function_ref specialized pullback of branch(_:_:)
// FINAL-NEXT:    %[[#ROOT:]] = function_ref
// FINAL-NEXT:    %[[#PB:]] = partial_apply {{.*}} %[[#ROOT]](
// FINAL-NOT:     partial_apply
// FINAL:         return %[[#PB]]
// FINAL-LABEL: {{^}}// specialized pullback of branch(_:_:)
// FINAL-NOT:     apply {{.*}} : $@callee_guaranteed
// FINAL:       } // end sil function

public struct Measurement: Differentiable {
  var value: Float
  @noDerivative var label: Any
}

@differentiable(reverse, wrt: x)
public func measuredSquare(_ x: Float, _ label: Any) -> Measurement {
  Measurement(value: x * x, label: label)
}

@inline(__always)
func pullbackOnValue(_ body: @escaping @Sendable (Float) -> Float)
  -> @Sendable (Measurement.TangentVector) -> Float {
  { seed in body(seed.value) }
}

@derivative(of: measuredSquare, wrt: x)
public func vjpMeasuredSquare(_ x: Float, _ label: Any)
  -> (value: Measurement, pullback: (Measurement.TangentVector) -> Float) {
  let body = pullbackOnValue { 2 * x * $0 }
  let callback: (Measurement.TangentVector) -> Float = body
  return (measuredSquare(x, label), callback)
}

// Exercise an indirect primal result and debug uses on both sides of the
// returned pullback's @Sendable conversion.
// CONVERTED-LABEL: {{^}}// vjpMeasuredSquare(_:_:)
// CONVERTED:       sil {{.*}} -> (@out Measurement,
// CONVERTED-NOT:     partial_apply
// CONVERTED:         // function_ref specialized closure #1 in pullbackOnValue(_:)
// CONVERTED-NEXT:    %[[#MEASURED:]] = function_ref
// CONVERTED-NEXT:    %[[#PB:]] = partial_apply {{.*}} %[[#MEASURED]](
// CONVERTED-NEXT:  debug_value %[[#PB]], let, name "body"
// CONVERTED-NEXT:    %[[#CONVERTED:]] = convert_function %[[#PB]] to $@callee_guaranteed
// CONVERTED-NEXT:  debug_value %[[#CONVERTED]], let, name "callback"
// CONVERTED-NOT:     partial_apply
// CONVERTED:         return %[[#CONVERTED]]
