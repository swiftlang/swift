// {"kind":"typecheck","original":"754fcb14","signature":"(anonymous namespace)::ApplyClassifier::classifyParameterBody(swift::ParamDecl*, swift::SubstitutionMap, (anonymous namespace)::PotentialEffectReason, swift::EffectKind)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ApplyClassifier::classifyApply"}
// RUN: not --crash %target-swift-frontend -typecheck %s
init<a>(b: a) where a == () throws -> String {
  b(
