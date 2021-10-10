// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK: sr10532.(file).ScalarProtocol@
// CHECK-NEXT: Requirement signature: <Self where Self : ScalarMultiplicative, Self == Self.Scalar>
public protocol ScalarProtocol: ScalarMultiplicative where Self == Scalar { }

// CHECK: sr10532.(file).ScalarMultiplicative@
// CHECK-NEXT: Requirement signature: <Self where Self.Scalar : ScalarProtocol>
public protocol ScalarMultiplicative {
  associatedtype Scalar : ScalarProtocol
}

// CHECK: sr10532.(file).MapReduceArithmetic@
// CHECK-NEXT: Requirement signature: <Self where Self : Collection, Self : ScalarMultiplicative, Self.Element : ScalarMultiplicative, Self.Scalar == Self.Element.Scalar>
public protocol MapReduceArithmetic : ScalarMultiplicative, Collection where Element : ScalarMultiplicative, Scalar == Element.Scalar { }

// CHECK: sr10532.(file).Tensor@
// CHECK-NEXT: Requirement signature: <Self where Self : MapReduceArithmetic, Self.Element : BinaryFloatingPoint, Self.Element : ScalarProtocol>
public protocol Tensor : MapReduceArithmetic where Scalar : BinaryFloatingPoint, Element == Scalar {
  var magnitude: Scalar { get set }
}

extension Tensor {
  public var magnitude: Scalar {
    return self.reduce(0) { $0 + $1 * $1 }.squareRoot()
  }
}
