// {"kind":"typecheck","original":"19492e52","signature":"swift::constraints::ConstraintSystem::setTargetFor(swift::constraints::SyntacticElementTargetKey, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (inserted), function setTargetFor","signatureNext":"ConstraintSystem::generateConstraints"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a {
  func c(d: a) {
    switch d {
    case let b:
      e {
        switch b {
        case .f(
          switch <#expression#> {
          }):
        }
      }
    }
  }
}
