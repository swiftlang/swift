// RUN: %target-swift-frontend -emit-sil -verify -primary-file %s -o /dev/null

import _Differentiation

/// Minimal reproducer for both single and double curry thunk

@inlinable
func caller<Thing: Differentiable & FloatingPoint>(
  of f: @differentiable(reverse) (_: Thing) -> Thing
) -> Int  where Thing.TangentVector == Thing {
   return 42
}

public struct Struct<Thing: Differentiable & FloatingPoint>: Differentiable where Thing.TangentVector == Thing {
    @inlinable
    static func foo_single() -> Int {
        return caller(of: callee_single) // No error expected
    }

    @inlinable
    @differentiable(reverse)
    static func callee_single(input: Thing) -> Thing {
        return input
    }

    @inlinable
    func foo_double() -> Int {
        return caller(of: callee_double) // No error expected
    }

    @inlinable
    @differentiable(reverse)
    func callee_double(input: Thing) -> Thing {
        return input
    }
}

/// Reproducer from https://github.com/swiftlang/swift/issues/75776

public struct Solution2<Thing: Differentiable & FloatingPoint>: Differentiable where Thing.TangentVector == Thing {
    @inlinable
    public static func optimization() -> Thing {
        var initial = Thing.zero
        let (_, delta) = valueWithGradient(at: initial, of: simulationWithLoss) // No error expected
        initial.move(by: delta)
        return initial
    }

    @inlinable
    @differentiable(reverse)
    static func simulationWithLoss(input: Thing) -> Thing {
        return input // implementation
    }
}

/// Reproducer from https://github.com/swiftlang/swift/issues/54819

public struct TF_688_Struct<Scalar> {
  var x: Scalar
}
extension TF_688_Struct: Differentiable where Scalar: Differentiable {
  @differentiable(reverse)
  public static func id(x: Self) -> Self {
    return x
  }
}
@differentiable(reverse, wrt: x)
public func TF_688<Scalar: Differentiable>(
  _ x: TF_688_Struct<Scalar>,
  reduction: @differentiable(reverse) (TF_688_Struct<Scalar>) -> TF_688_Struct<Scalar> = TF_688_Struct.id // No error expected
) -> TF_688_Struct<Scalar> {
  reduction(x)
}
