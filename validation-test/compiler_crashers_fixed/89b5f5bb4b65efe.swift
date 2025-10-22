// {"kind":"typecheck","original":"01146903","signature":"(anonymous namespace)::ConstraintWalker::walkToExprPost(swift::Expr*)"}
// RUN: not %target-swift-frontend -typecheck %s
switch <#expression#> {
case let .a -> b:
  b
}
