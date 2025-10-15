// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::getTypeOfMemberReferencePre(swift::constraints::OverloadChoice, swift::DeclContext*, swift::constraints::ConstraintLocator*, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a where b == Self
  protocol c : a where Self
  : CustomStringConvertible
    var d e c.description
