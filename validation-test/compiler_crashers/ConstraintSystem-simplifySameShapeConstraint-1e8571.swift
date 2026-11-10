// {"kind":"typecheck","original":"5df04952","signature":"swift::constraints::ConstraintSystem::simplifySameShapeConstraint(swift::Type, swift::Type, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ConstraintSystem::simplifyConstraint"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b, each d>(
  repeat each b, c: repeat each d -> (repeat (each b each d
func e<each g>(f: repeat each g) {
  a(repeat f
