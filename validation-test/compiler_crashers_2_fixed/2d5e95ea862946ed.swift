// {"kind":"typecheck","signature":"(anonymous namespace)::ExprWalker::walkToExprPost(swift::Expr*)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->"}
// RUN: not %target-swift-frontend -typecheck %s
func a {
  {
    \ b() a
