// {"kind":"typecheck","original":"d4d10d57","signature":"swift::constraints::ContextualFailure::diagnoseConversionToBool() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ArgumentMismatchFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<let b: Int> {
  c {
  !b
  }
