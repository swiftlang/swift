// {"kind":"emit-silgen","signature":"swift::SILArgument::SILArgument(swift::ValueKind, swift::SILBasicBlock*, swift::SILType, swift::ValueOwnershipKind, swift::ValueDecl*, bool, bool)","signatureAssert":"Assertion failed: (!type.hasTypeParameter()), function SILArgument","signatureNext":"SILBasicBlock::createPhiArgument"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
import _Differentiation
class a<b: Differentiable> {
  @differentiable()
  func c(d: b) throws -> b {
  }
}
