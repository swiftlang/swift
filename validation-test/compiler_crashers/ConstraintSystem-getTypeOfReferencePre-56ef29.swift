// {"kind":"typecheck","original":"8ab05958","signature":"swift::constraints::ConstraintSystem::getTypeOfReferencePre(swift::constraints::OverloadChoice, swift::DeclContext*, swift::constraints::ConstraintLocatorBuilder, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConstraintSystem::prepareOverloadImpl"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@freestanding(declaration arbitrary) macro a< d >(d =
enum b {
  case c
}
#a(b.c
