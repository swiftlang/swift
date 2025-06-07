// {"signature":"(anonymous namespace)::PreCheckTarget::walkToExprPre(swift::Expr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  extension a {
    b {
      func c {
        super
