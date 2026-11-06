// {"kind":"typecheck","original":"5f61c0c1","signature":"swift::constraints::RequirementFailure::getDeclRef() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"RequirementFailure::RequirementFailure"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@resultBuilder struct a {
  static
  buildBlock->[Equatable]
}
func b(@a()   -> Equatable)b {
