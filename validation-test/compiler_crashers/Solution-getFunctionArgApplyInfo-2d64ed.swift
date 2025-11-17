// {"kind":"typecheck","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
    callAsFunction( ())
  {
    a {
