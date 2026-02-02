// {"kind":"typecheck","signature":"(anonymous namespace)::PreCheckTarget::walkToExprPre(swift::Expr*)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  extension a {
    b {
      func c {
        super
