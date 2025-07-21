// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::recordResolvedOverload(swift::constraints::ConstraintLocator*, swift::constraints::SelectedOverload)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a func b(c : a) {
  {
    switch
      c { case .baz(c==
