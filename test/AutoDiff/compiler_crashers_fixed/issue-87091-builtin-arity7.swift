// RUN: %target-swift-frontend-typecheck -parse-stdlib %s

import Swift
import _Differentiation

// https://github.com/swiftlang/swift/issues/87091
// Ensure we can create builtins with more than 6 generic parameters

@inlinable
public func valueWithPullback<A, B, C, D, E, F, G, H, Result>(
    at a: A, _ b: B, _ c: C, _ d: D, _ e: E, _ f: F, _ g: G, _ h: H, of body: @differentiable(reverse) (A, B, C, D, E, F, G, H) -> Result
) -> (value: Result,
      pullback: (Result.TangentVector)
      -> (A.TangentVector, B.TangentVector, C.TangentVector, D.TangentVector, E.TangentVector, F.TangentVector, G.TangentVector, H.TangentVector)) {
    return Builtin.applyDerivative_vjp_arity8(body, a, b, c, d, e, f, g, h)
}
