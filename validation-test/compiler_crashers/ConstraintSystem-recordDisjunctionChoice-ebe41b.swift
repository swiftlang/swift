// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::recordDisjunctionChoice(swift::constraints::ConstraintLocator*, unsigned int)","signatureAssert":"Assertion failed: (inserted), function recordDisjunctionChoice","signatureNext":"DisjunctionStep::attempt"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  init! ( b: Int, () -> Int = {
    }
() -> Int = a(b : 1) {
