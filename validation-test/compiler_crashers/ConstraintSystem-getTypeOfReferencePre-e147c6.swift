// {"kind":"typecheck","original":"d82f42f8","signature":"swift::constraints::ConstraintSystem::getTypeOfReferencePre(swift::constraints::OverloadChoice, swift::DeclContext*, swift::constraints::ConstraintLocatorBuilder, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConstraintSystem::getTypeOfMemberReferencePre"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a
  subscript -> a
  let b ~main  [
