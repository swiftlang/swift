// RUN: %target-swift-frontend -emit-sil -verify %s

// rdar://87429620
// https://github.com/apple/swift/issues/54819
// Differentiable curry thunk RequirementMachine error

import _Differentiation

public struct Struct<Scalar> {
  var x: Scalar
}

extension Struct: Differentiable where Scalar: Differentiable {
  @differentiable(reverse)
  public static func id(x: Self) -> Self {
    return x
  }
}

@differentiable(reverse, wrt: x)
public func f<Scalar: Differentiable>(
  _ x: Struct<Scalar>,
  reduction: @differentiable(reverse) (Struct<Scalar>) -> Struct<Scalar> = Struct.id
) -> Struct<Scalar> {
  reduction(x)
}

// Invalid type parameter in getCanonicalTypeInContext()
// Original type: τ_0_0.TangentVector
// Simplified term: τ_0_0.[Differentiable:TangentVector]
// Longest valid prefix: τ_0_0
// Prefix type: τ_0_0
//
// Requirement machine for <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 == τ_0_1, τ_0_2 == τ_0_3>
// Rewrite system: {
// - τ_0_1 => τ_0_0 [explicit]
// - τ_0_3 => τ_0_2 [explicit]
// }
// Rewrite loops: {
// }
// Property map: {
// }
// Conformance access paths: {
// }
