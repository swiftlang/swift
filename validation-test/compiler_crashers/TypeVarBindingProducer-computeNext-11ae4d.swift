// {"kind":"typecheck","original":"1ce6550f","signature":"swift::constraints::TypeVarBindingProducer::computeNext()","signatureAssert":"Assertion failed: (Kind != AllowedBindingKind::Fallback), function withType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
withCheckedThrowingContinuation {
  a {
  }!
}
