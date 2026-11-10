// {"kind":"typecheck","original":"4472d299","signature":"(anonymous namespace)::OverrideMatcher::match((anonymous namespace)::OverrideCheckingAttempt)","signatureAssert":"Assertion failed: (parentMethod || parentStorage), function match","signatureNext":"computeOverriddenDecls"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a: b
  protocol b: a {
    macro c
