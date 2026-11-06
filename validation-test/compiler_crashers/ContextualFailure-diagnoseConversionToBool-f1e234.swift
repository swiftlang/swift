// {"kind":"typecheck","original":"bc93ac2b","signature":"swift::constraints::ContextualFailure::diagnoseConversionToBool() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ContextualFailure::diagnoseAsError"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<let b: Int> {
  c {
  guard b
  }
