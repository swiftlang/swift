// {"kind":"typecheck","original":"8e102ce3","signature":"swift::TypeResolution::areSameType(swift::Type, swift::Type) const","signatureAssert":"Assertion failed: (stage == TypeResolutionStage::Structural), function areSameType","signatureNext":"TypeResolver::resolveDeclRefTypeReprRec"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct b {
  @d func a<d, d>()
}
