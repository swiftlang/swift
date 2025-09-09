// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::recordResolvedOverload(swift::constraints::ConstraintLocator*, swift::constraints::SelectedOverload)","signatureAssert":"Assertion failed: (inserted), function recordResolvedOverload"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a func b(c : a) {
  {
    switch
      c { case .baz(c==
