// {"kind":"typecheck","signature":"swift::GenericSignatureImpl::isRequirementSatisfied(swift::Requirement, bool, bool) const","signatureAssert":"Assertion failed: (false && \"Refactor this\"), function isRequirementSatisfied","signatureNext":"matchWitness"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
import _Differentiation
protocol P {
  @differentiable(reverse)
  func foo(_: Float) -> Float
}
struct S<each T>: P {
  @differentiable(reverse)
  func foo(_ x: Float) -> Float {
    x
  }
}
