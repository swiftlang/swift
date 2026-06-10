// {"kind":"typecheck","original":"e71e36c1","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"TreatEphemeralAsNonEphemeral::diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a
  struct b {
    c: a
    d: UnsafePointer<Int8>
    let e = b(
      (0
    ""
