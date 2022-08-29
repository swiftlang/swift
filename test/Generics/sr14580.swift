// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/56932

public protocol ScalarProtocol: ScalarMultiplicative where Self == Scalar {
}

public protocol ScalarMultiplicative {
  associatedtype Scalar: ScalarProtocol
}

public protocol MapReduceArithmetic: ScalarMultiplicative, Collection where Element: ScalarMultiplicative {}

public protocol Tensor: MapReduceArithmetic where Element == Scalar {
}

// CHECK-LABEL: sr14580.(file).ColorModel@
// CHECK-LABEL: Requirement signature: <Self where Self : Tensor, Self == Self.[ColorModel]Float16Components.[ColorComponents]Model, Self.[Sequence]Element == Double, Self.[ColorModel]Float16Components : ColorComponents, Self.[ColorModel]Float32Components : ColorComponents, Self.[ColorModel]Float16Components.[Sequence]Element == Double, Self.[ColorModel]Float16Components.[ColorComponents]Model == Self.[ColorModel]Float32Components.[ColorComponents]Model, Self.[ColorModel]Float32Components.[Sequence]Element == Double>
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
