// {"kind":"emit-silgen","signature":"(anonymous namespace)::ImmutableAddressUseVerifier::isConsumingOrMutatingApplyUse(swift::Operand*)","signatureAssert":"Assertion failed: (conv.isIndirectConvention() && \"Expect an indirect convention\"), function isConsumingOrMutatingArgumentConvention","signatureNext":"ImmutableAddressUseVerifier::isMutatingOrConsuming"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
import _Differentiation
public func c<d>(a: d, b: d) -> d {
}
@derivative(of: c)
public func e<d: Differentiable & AnyObject>(a: d, b: d)
  -> (value: d, pullback: (d.TangentVector) -> (d.TangentVector, d.TangentVector))
{
}
