// {"kind":"typecheck","original":"8c042bb8","signature":"swift::constraints::NoopCheckedCast::NoopCheckedCast(swift::constraints::Solution const&, swift::Type, swift::Type, swift::CheckedCastKind, swift::constraints::ConstraintLocator*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"AllowNoopCheckedCast::diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>() {
  if case b = c as b {
  }
}
