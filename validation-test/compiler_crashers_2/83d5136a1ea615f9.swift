// {"signature":"swift::constraints::ConstraintSystem::resolveOverload(swift::constraints::ConstraintLocator*, swift::Type, swift::constraints::OverloadChoice, swift::DeclContext*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a func b(c : a) {
  {
    switch
      c {                       case .baz(c==
