// {"kind":"typecheck","signature":"swift::ExistentialArchetypeType::get(swift::CanType)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConstraintSystem::getTypeOfMemberReferencePre"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a where b == Self
  protocol c : a where Self
  : CustomStringConvertible
    var d e c.description
