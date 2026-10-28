// {"kind":"typecheck","original":"6064f00b","signature":"swift::constraints::ConstraintSystem::setTargetFor(swift::constraints::SyntacticElementTargetKey, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (inserted), function setTargetFor","signatureNext":"ConstraintSystem::generateConstraints"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(b: Never) {
  c =
    switch b {
    case .d(
      switch <#expression#> {
      }):
    }
}
