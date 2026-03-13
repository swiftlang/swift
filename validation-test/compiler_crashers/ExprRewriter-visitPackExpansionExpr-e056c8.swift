// {"kind":"typecheck","original":"ee11d1cb","signature":"(anonymous namespace)::ExprRewriter::visitPackExpansionExpr(swift::PackExpansionExpr*)","signatureAssert":"Assertion failed: (environment), function visitPackExpansionExpr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  repeat [each $0]
}
