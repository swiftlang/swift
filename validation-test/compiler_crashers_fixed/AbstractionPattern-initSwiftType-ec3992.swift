// {"kind":"emit-silgen","signature":"swift::Lowering::AbstractionPattern::initSwiftType(swift::SubstitutionMap, swift::CanGenericSignature, swift::CanType, swift::Lowering::AbstractionPattern::Kind)","signatureAssert":"Assertion failed: (signature || !origType->hasTypeParameter()), function initSwiftType","signatureNext":"TypeClassifierBase::visitStructType"}
// RUN: not %target-swift-frontend -emit-silgen %s
class a {
  func b(c: (Int) -> Void) {
  }
}
class d: a {
  override func b(c: (Int...) -> Void) {
  }
}
