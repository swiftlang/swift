// {"kind":"typecheck","original":"6eb456ed","signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"AllowArgumentMismatch::diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < b {
callAsFunction<c>(d : b) {
\ a<c>(d)
