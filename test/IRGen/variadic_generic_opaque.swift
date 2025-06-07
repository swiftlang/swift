// RUN: %target-swift-frontend -emit-ir %s -target %target-swift-5.9-abi-triple | %FileCheck %s

public protocol P {}

public struct G<each T>: P {}

public func concreteG<each T>(_ t: repeat each T) -> some P {
  return G<repeat each T>()
}

public func abstractG<T>(_ t: T) -> some P {
  return G<T>()
}

public func variadicG<each T>(_ t: repeat each T) -> some P {
  return G<repeat each T>()
}

// Opaque return type is witnessed by a conditional conformance
protocol Q {}

struct S1: Q {}
struct S2: Q {}

struct G2<each T> {}
extension G2: P where repeat each T: Q {}

func concreteG2() -> some P {
  G2<S1, S2>()
}

func abstractG2<T: Q>(_: T) -> some P {
  G2<S1, T>()
}

func variadicG2<each T: Q>(_: repeat each T) -> some P {
  G2<repeat each T>()
}

// CHECK: define private ptr @"get_witness_table 23variadic_generic_opaque2G2VyAA2S1V_AA2S2VQPGAA1PHPAeA1QHPyHC_AgaJHPyHCHX_HC"
// CHECK: define private ptr @"get_witness_table 23variadic_generic_opaque1QRzlAA2G2VyAA2S1V_xQPGAA1PHPAfaBHPyHC_xAaBHD1_HX_HC"
// CHECK: define private ptr @"get_witness_table Rvz23variadic_generic_opaque1QRzlAA2G2VyxxQp_QPGAA1PHPxAaBHD1__HX_HC"

// Conditional same-shape requirement
public struct Outer<each T> {
  public struct Inner<each U> {}
}

extension Outer.Inner: P where (repeat (each T, each U)): Any {}

func concreteOuterInner() -> some P {
  Outer<Int>.Inner<String>()
}
