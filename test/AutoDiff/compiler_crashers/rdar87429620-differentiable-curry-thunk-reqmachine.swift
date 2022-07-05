// RUN: %target-swift-frontend -emit-sil -verify %s
// XFAIL: *

// rdar://87429620 (Differentiable curry thunk RequirementMachine error)

import _Differentiation

public struct SR_14228_Struct<Scalar> {
  var x: Scalar
}

extension SR_14228_Struct: Differentiable where Scalar: Differentiable {
  @differentiable(reverse)
  public static func id(x: Self) -> Self {
    return x
  }
}

@differentiable(reverse, wrt: x)
public func SR_14228<Scalar: Differentiable>(
  _ x: SR_14228_Struct<Scalar>,
  reduction: @differentiable(reverse) (SR_14228_Struct<Scalar>) -> SR_14228_Struct<Scalar> = SR_14228_Struct.id
) -> SR_14228_Struct<Scalar> {
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
