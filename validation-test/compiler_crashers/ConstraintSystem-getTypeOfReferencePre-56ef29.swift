// {"kind":"typecheck","original":"8ab05958","signature":"swift::constraints::ConstraintSystem::getTypeOfReferencePre(swift::constraints::OverloadChoice, swift::DeclContext*, swift::constraints::ConstraintLocatorBuilder, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConstraintSystem::prepareOverloadImpl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@freestanding(declaration arbitrary) macro a< d >(d =
enum b {
  case c
}
#a(b.c
