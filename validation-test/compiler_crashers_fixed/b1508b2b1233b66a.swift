// {"kind":"typecheck","original":"001363f2","signature":"swift::ASTContext::getSpecializedConformance(swift::Type, swift::NormalProtocolConformance*, swift::SubstitutionMap)","signatureAssert":"Assertion failed: (substitutions.getGenericSignature().getCanonicalSignature() == generic->getGenericSignature().getCanonicalSignature()), function getSpecializedConformance"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  associatedtype  1
}
protocol b {
  associatedtype c: a where d == Self
  class e<f>: a where f: b, f.c == Self {
  }
}
