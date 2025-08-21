// {"kind":"typecheck","signature":"swift::ASTContext::getSpecializedConformance(swift::Type, swift::NormalProtocolConformance*, swift::SubstitutionMap)","signatureAssert":"Assertion failed: (substitutions.getGenericSignature().getCanonicalSignature() == generic->getGenericSignature().getCanonicalSignature()), function getSpecializedConformance"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a{ < } protocol b { associatedtype c : a where d == Self }
class e<f> : a where f : b, f.c == e
