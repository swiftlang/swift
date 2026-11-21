// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name DirectReturn %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

// CHECK-LABEL: {{^}}// reverse-mode derivative of inplaceScaledSquare
// CHECK-NOT:     partial_apply
// CHECK:       witness_method {{.*}} #CoefficientProvider.coefficient
// CHECK-NOT:     partial_apply
// CHECK:         // function_ref specialized pullback of inplaceScaledSquare
// CHECK-NEXT:    %[[#SQUARE_FN:]] = function_ref @[[SQUARE_SYM:[^ ]+]]
// CHECK-NEXT:    %[[#SQUARE_PB:]] = partial_apply {{.*}} %[[#SQUARE_FN]](
// CHECK-NEXT:    return %[[#SQUARE_PB]]
// CHECK-NEXT:  } // end sil function
// CHECK:       sil private @[[SQUARE_SYM]] :
// CHECK-NOT:     partial_apply
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// reverse-mode derivative of inplaceSignedScale
// CHECK-NOT:     partial_apply
// CHECK:       witness_method {{.*}} #CoefficientProvider.coefficient
// CHECK-NOT:     partial_apply
// CHECK:         // function_ref specialized pullback of inplaceSignedScale
// CHECK-NEXT:    %[[#SCALE_FN:]] = function_ref @[[SCALE_SYM:[^ ]+]]
// CHECK-NEXT:    %[[#SCALE_PB:]] = partial_apply {{.*}} %[[#SCALE_FN]](
// CHECK-NEXT:    return %[[#SCALE_PB]]
// CHECK-NEXT:  } // end sil function
// CHECK:       sil private @[[SCALE_SYM]] :
// CHECK-NOT:     partial_apply
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function

// An address-only primal result also leaves the pullback as the only direct result.
// CHECK-LABEL: {{^}}// reverse-mode derivative of packetSignedScale
// CHECK:       sil {{.*}} -> (@out Packet, @owned @callee_guaranteed
// CHECK-NOT:     partial_apply
// CHECK:       witness_method {{.*}} #CoefficientProvider.coefficient
// CHECK-NOT:     partial_apply
// CHECK:         // function_ref specialized pullback of packetSignedScale
// CHECK-NEXT:    %[[#PACKET_FN:]] = function_ref @[[PACKET_SYM:[^ ]+]]
// CHECK-NEXT:    %[[#PACKET_PB:]] = partial_apply {{.*}} %[[#PACKET_FN]](
// CHECK-NEXT:    return %[[#PACKET_PB]]
// CHECK-NEXT:  } // end sil function
// CHECK:       sil private @[[PACKET_SYM]] :
// CHECK-NOT:     partial_apply
// CHECK-NOT:     apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function

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
