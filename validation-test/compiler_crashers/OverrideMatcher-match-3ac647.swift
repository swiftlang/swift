// {"kind":"typecheck","original":"4472d299","signature":"(anonymous namespace)::OverrideMatcher::match((anonymous namespace)::OverrideCheckingAttempt)","signatureAssert":"Assertion failed: (parentMethod || parentStorage), function match","signatureNext":"computeOverriddenDecls"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a: b
  protocol b: a {
    macro c
