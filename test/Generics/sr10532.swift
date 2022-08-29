// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/52932

// CHECK: sr10532.(file).ScalarProtocol@
// CHECK-NEXT: Requirement signature: <Self where Self : ScalarMultiplicative, Self == Self.[ScalarMultiplicative]Scalar>
public protocol ScalarProtocol: ScalarMultiplicative where Self == Scalar { }

// CHECK: sr10532.(file).ScalarMultiplicative@
// CHECK-NEXT: Requirement signature: <Self where Self.[ScalarMultiplicative]Scalar : ScalarProtocol>
public protocol ScalarMultiplicative {
  associatedtype Scalar : ScalarProtocol
}

// CHECK: sr10532.(file).MapReduceArithmetic@
// CHECK-NEXT: Requirement signature: <Self where Self : Collection, Self : ScalarMultiplicative, Self.[Sequence]Element : ScalarMultiplicative, Self.[ScalarMultiplicative]Scalar == Self.[Sequence]Element.[ScalarMultiplicative]Scalar>
public protocol MapReduceArithmetic : ScalarMultiplicative, Collection where Element : ScalarMultiplicative, Scalar == Element.Scalar { }

// CHECK: sr10532.(file).Tensor@
// CHECK-NEXT: Requirement signature: <Self where Self : MapReduceArithmetic, Self.[Sequence]Element : BinaryFloatingPoint, Self.[Sequence]Element == Self.[ScalarMultiplicative]Scalar>
public protocol Tensor : MapReduceArithmetic where Scalar : BinaryFloatingPoint, Element == Scalar {
  var magnitude: Scalar { get set }
}

extension Tensor {
  public var magnitude: Scalar {
    return self.reduce(0) { $0 + $1 * $1 }.squareRoot()
  }
}
