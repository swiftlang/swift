// {"kind":"typecheck","signature":"(anonymous namespace)::ExprRewriter::visitDeclRefExpr(swift::DeclRefExpr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  for b  0..<0 {
    let a = Array(0..<b)
    for c  d    a {
      for e  d      c... {
        Array(a[c..<e].reversed())
        f(
