// {"kind":"typecheck","original":"b7e55082","signature":"swift::ExistentialArchetypeType::getAny(swift::Type)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConstraintSystem::openAnyExistentialType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a where b == Self
  func c(d: a) {
    d == d
