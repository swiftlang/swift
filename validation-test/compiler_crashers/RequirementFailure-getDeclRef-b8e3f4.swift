// {"kind":"typecheck","original":"5f61c0c1","signature":"swift::constraints::RequirementFailure::getDeclRef() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"RequirementFailure::RequirementFailure"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@resultBuilder struct a {
  static
  buildBlock->[Equatable]
}
func b(@a()   -> Equatable)b {
