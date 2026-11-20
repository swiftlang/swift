// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name TangentSwitch %s -Xllvm -sil-print-around=autodiff-closure-specialization -o /dev/null 2> %t.passes
// RUN: %FileCheck %s < %t.passes

import _Differentiation

public enum Delta: Differentiable, AdditiveArithmetic {
  public typealias TangentVector = Self
  case zero, one, value(Float)

  var scalar: Float {
    switch self {
    case .zero: return 0
    case .one: return 1
    case let .value(x): return x
    }
  }

  @inline(__always)
  public static func + (lhs: Self, rhs: Self) -> Self {
    switch lhs {
    case .zero: return rhs
    default: return .value(lhs.scalar + rhs.scalar)
    }
  }

  public static func - (lhs: Self, rhs: Self) -> Self { .value(lhs.scalar - rhs.scalar) }
  public static func == (lhs: Self, rhs: Self) -> Bool { lhs.scalar == rhs.scalar }
}

public struct Scalar: Differentiable {
  public var value: Float
  public typealias TangentVector = Delta
  public mutating func move(by offset: Delta) { value += offset.scalar }
}

@differentiable(reverse)
public func scalarValue(_ x: Scalar) -> Float { x.value }

@derivative(of: scalarValue)
@inline(__always)
public func vjpScalarValue(_ x: Scalar) -> (value: Float, pullback: (Float) -> Delta) {
  (x.value, { .value($0) })
}

@differentiable(reverse, wrt: x)
public func branch(_ x: Scalar, _ flag: Bool) -> Float {
  if flag { return scalarValue(x) }
  return 0
}

// The tangent switch is unrelated to the branch tape. Its default edge must
// survive cloning the pullback.
// CHECK-LABEL: // function_ref specialized pullback of branch(_:_:)
// CHECK:       // specialized pullback of branch(_:_:)
// CHECK:         // function_ref closure #1 in vjpScalarValue(_:)
// CHECK-NEXT:    %[[#CALLBACK:]] = function_ref
// CHECK-NEXT:    %[[#DELTA:]] = apply %[[#CALLBACK]](
// CHECK:         switch_enum %[[#DELTA]], case #Delta.zero!enumelt: bb[[#]], default bb[[#]]
