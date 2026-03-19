// {"kind":"typecheck","original":"a0565833","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"getStructuralTypeContext"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b {
  base: b
  func callAsFunction(b  -> b) -> b
}
a("") {
  $0.isEmpty
} {
