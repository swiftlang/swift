// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

public protocol ScalarProtocol: ScalarMultiplicative where Self == Scalar {
}

public protocol ScalarMultiplicative {
  associatedtype Scalar: ScalarProtocol
}

public protocol MapReduceArithmetic: ScalarMultiplicative, Collection where Element: ScalarMultiplicative {}

public protocol Tensor: MapReduceArithmetic where Element == Scalar {
}

// FIXME: The same-type requirement canonicalization still produces bugus results here, but
// at least we don't crash.

// CHECK-LABEL: Requirement signature: <Self where Self : Tensor, Self == Self.Float16Components.Model, Self.Element == Double, Self.Float16Components : ColorComponents, Self.Float32Components : ColorComponents, Self.Float16Components.Element == Double, Self.Float16Components.Model == Self.Float32Components.Model, Self.Float32Components.Element == Double, Self.Float32Components.Indices == Self.Float32Components.Indices.SubSequence, Self.Float32Components.SubSequence == Self.Float32Components.SubSequence.SubSequence, Self.Float32Components.Indices.Indices == Self.Float32Components.Indices.Indices.SubSequence, Self.Float32Components.SubSequence.Indices == Self.Float32Components.SubSequence.Indices.SubSequence, Self.Float32Components.SubSequence.Indices.Indices == Self.Float32Components.SubSequence.Indices.Indices.SubSequence>
public protocol ColorModel: Tensor where Scalar == Double {
  associatedtype Float16Components: ColorComponents where Float16Components.Model == Self, Float16Components.Scalar == Double
  associatedtype Float32Components: ColorComponents where Float32Components.Model == Self, Float32Components.Scalar == Double
}

public protocol ColorComponents: Tensor {
  associatedtype Model: ColorModel
}

extension Double : ScalarMultiplicative {}
extension Double : ScalarProtocol {
  public typealias Scalar = Self
}
