// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-Onone -Xfrontend -sil-verify-all)
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name src %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation

public protocol CoefficientProvider {
  func coefficient() -> Float
}

@differentiable(reverse, wrt: x)
public func scaledSquare(_ x: Float, _ provider: any CoefficientProvider) -> Float {
  let coefficient = provider.coefficient()
  return coefficient * x * x
}

@differentiable(reverse, wrt: x)
public func scaled(_ x: Float, _ provider: any CoefficientProvider) -> Float {
  provider.coefficient() * x
}

@differentiable(reverse, wrt: x)
public func inplaceScaledSquare(
  _ x: inout Float, _ flipSign: Bool, _ provider: any CoefficientProvider
) {
  if flipSign { x = -x }
  x = scaledSquare(x, provider)
}

@differentiable(reverse, wrt: x)
public func inplaceSignedScale(
  _ x: inout Float, _ flipSign: Bool, _ provider: any CoefficientProvider
) {
  if flipSign { x = -x }
  x = scaled(x, provider)
}

public struct Packet: Differentiable {
  public var value: Float
  @noDerivative public var label: Any

  @differentiable(reverse, wrt: value)
  public init(_ value: Float, _ label: Any) {
    self.value = value
    self.label = label
  }
}

@differentiable(reverse, wrt: x)
public func packetSignedScale(
  _ x: Float, _ flipSign: Bool, _ provider: any CoefficientProvider, _ label: Any
) -> Packet {
  let y = flipSign ? -x : x
  return Packet(scaled(y, provider), label)
}

// Inout mutations and address-only results leave the pullback as the only
// direct result.
// CHECK-LABEL: // reverse-mode derivative of packetSignedScale(_:_:_:_:)
// CHECK:       sil {{.*}} -> (@out Packet, @owned @callee_guaranteed
// CHECK-NOT:     = partial_apply
// CHECK:         witness_method {{.*}} #CoefficientProvider.coefficient
// CHECK-NOT:     = partial_apply
// CHECK:         // function_ref specialized pullback of packetSignedScale(_:_:_:_:)
// CHECK-NEXT:    %[[#PACKET_FN:]] = function_ref
// CHECK-NEXT:    %[[#PACKET_PB:]] = partial_apply [callee_guaranteed] %[[#PACKET_FN]]
// CHECK-NEXT:    return %[[#PACKET_PB]]
// CHECK-NEXT:  } // end sil function

// CHECK-LABEL: // specialized reverse-mode derivative of inplaceSignedScale(_:_:_:)
// CHECK-NOT:     = partial_apply
// CHECK:         witness_method {{.*}} #CoefficientProvider.coefficient
// CHECK-NOT:     = partial_apply
// CHECK:         // function_ref specialized pullback of inplaceSignedScale(_:_:_:)
// CHECK-NEXT:    %[[#SCALE_FN:]] = function_ref
// CHECK-NEXT:    %[[#SCALE_PB:]] = partial_apply [callee_guaranteed] %[[#SCALE_FN]]
// CHECK-NEXT:    return %[[#SCALE_PB]]
// CHECK-NEXT:  } // end sil function

// CHECK-LABEL: // specialized reverse-mode derivative of inplaceScaledSquare(_:_:_:)
// CHECK-NOT:     = partial_apply
// CHECK:         witness_method {{.*}} #CoefficientProvider.coefficient
// CHECK-NOT:     = partial_apply
// CHECK:         // function_ref specialized pullback of inplaceScaledSquare(_:_:_:)
// CHECK-NEXT:    %[[#SQUARE_FN:]] = function_ref
// CHECK-NEXT:    %[[#SQUARE_PB:]] = partial_apply [callee_guaranteed] %[[#SQUARE_FN]]
// CHECK-NEXT:    return %[[#SQUARE_PB]]
// CHECK-NEXT:  } // end sil function

struct ConstantCoefficient: CoefficientProvider {
  let value: Float

  func coefficient() -> Float { value }
}

for coefficient: Float in [-2, 0, 0.5] {
  let provider = ConstantCoefficient(value: coefficient)
  for flipSign in [false, true] {
    for x: Float in [-1.5, 0, 2] {
      let square = valueWithPullback(at: x) { x in
        var y = x
        inplaceScaledSquare(&y, flipSign, provider)
        return y
      }
      let signed = valueWithPullback(at: x) { x in
        var y = x
        inplaceSignedScale(&y, flipSign, provider)
        return y
      }
      let packet = valueWithPullback(at: x) {
        packetSignedScale($0, flipSign, provider, 42)
      }
      let signedCoefficient = flipSign ? -coefficient : coefficient
      precondition(square.value == coefficient * x * x)
      precondition(signed.value == signedCoefficient * x)
      precondition(packet.value.value == signed.value)
      precondition(packet.value.label as? Int == 42)
      precondition(gradient(at: x) { packetSignedScale($0, flipSign, provider, 42).value }
                   == signedCoefficient)
      for seed: Float in [1, 0, -2] {
        precondition(square.pullback(seed) == 2 * coefficient * x * seed)
        precondition(signed.pullback(seed) == signedCoefficient * seed)
        precondition(packet.pullback(.init(value: seed)) == signedCoefficient * seed)
      }
    }
  }
}
