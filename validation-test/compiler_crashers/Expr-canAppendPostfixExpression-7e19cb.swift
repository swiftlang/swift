// {"kind":"typecheck","signature":"swift::Expr::canAppendPostfixExpression(bool) const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<let b: Int?{ int: Int { b
