// {"kind":"typecheck","original":"d4d10d57","signature":"swift::Expr::canAppendPostfixExpression(bool) const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ContextualFailure::diagnoseConversionToBool"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<let b: Int> {
  c {
  !b
  }
