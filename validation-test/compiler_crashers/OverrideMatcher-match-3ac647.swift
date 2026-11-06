// {"kind":"typecheck","original":"4472d299","signature":"(anonymous namespace)::OverrideMatcher::match((anonymous namespace)::OverrideCheckingAttempt)","signatureAssert":"Assertion failed: (parentMethod || parentStorage), function match","signatureNext":"computeOverriddenDecls"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a: b
  protocol b: a {
    macro c
