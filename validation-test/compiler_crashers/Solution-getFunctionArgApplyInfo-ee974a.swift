// {"kind":"typecheck","original":"2036470e","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"InvalidPackExpansion::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
sequence(
  (a
repeat a
