// RUN: %target-swift-frontend -emit-sil -verify %s

// https://github.com/apple/swift/pull/68777
// We used not to constrain the derivative type properly, 
// the `Differentiable` requirement was missed if the function type had
// pattern substitution resulting in assertion:
//
// Invalid type parameter in getReducedType()
// Original type: Optional<τ_0_0.TangentVector>
// Simplified term: τ_0_0.[Differentiable:TangentVector]
// Longest valid prefix: τ_0_0
// Prefix type: τ_0_0
//
// Requirement machine for <τ_0_0, τ_0_1>
// Rewrite system: {
// }
// }
// Property map: {
// }
// Conformance paths: {
// }
//
// Note that the generic signature <τ_0_0, τ_0_1> should be
// <τ_0_0 : Differentiable, τ_0_1 : Differentiable>, otherwise
// there is no way to derive associated type τ_0_0.TangentVector

import _Differentiation; 

public struct D<T>: Differentiable {}
extension D {@differentiable(reverse, wrt: self) mutating func m(r: @differentiable(reverse) (T?) -> T?) {}}
