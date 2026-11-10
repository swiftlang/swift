// {"kind":"typecheck","original":"b69560a8","signature":"swift::constraints::ArgumentMismatchFailure::diagnoseUseOfReferenceEqualityOperator() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ArgumentMismatchFailure::diagnoseAsError"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct Vector
  let b
  === a
struct a {
    === (b: Vector
