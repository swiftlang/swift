// {"kind":"typecheck","original":"3d641f3b","signature":"swift::ExistentialArchetypeType::get(swift::CanType)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ExistentialArchetypeType::getAny"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol c where d == Self
  func e(a: c b: c) {
    type(of: a) == type(of: b
