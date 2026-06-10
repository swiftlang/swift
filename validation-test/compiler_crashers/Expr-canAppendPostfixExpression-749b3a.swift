// {"kind":"typecheck","original":"f7236a3b","signature":"swift::Expr::canAppendPostfixExpression(bool) const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"VarDeclUsageChecker"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  class b {
    c { guard let d = c as? Any is a
      }
  }
