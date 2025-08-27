// {"kind":"typecheck","signature":"swift::ExistentialArchetypeType::get(swift::CanType)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a where b == Self
  protocol c : a where Self
  : CustomStringConvertible
    var d e c.description
