// {"signature":"(anonymous namespace)::ExprWalker::walkToExprPost(swift::Expr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a {
  {
    \ b() a
