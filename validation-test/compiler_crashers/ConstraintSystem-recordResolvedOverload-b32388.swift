// {"kind":"typecheck","original":"4b05db17","signature":"swift::constraints::ConstraintSystem::recordResolvedOverload(swift::constraints::ConstraintLocator*, swift::constraints::SelectedOverload)","signatureAssert":"Assertion failed: (inserted), function recordResolvedOverload","signatureNext":"ConstraintSystem::resolveOverload"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a {
  case b
  enum c  ; d(e: c??) {
    + switch e { case .some(.some(carriersome(b
      }
  }
