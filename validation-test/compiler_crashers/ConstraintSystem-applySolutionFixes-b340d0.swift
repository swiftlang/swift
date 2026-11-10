// {"kind":"typecheck","original":"e71e36c1","signature":"swift::constraints::ConstraintSystem::applySolutionFixes(swift::constraints::Solution const&)","signatureAssert":"Assertion failed: (diagnosed && \"warnings should always be diagnosed\"), function applySolutionFixes","signatureNext":"ConstraintSystem::applySolution"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a
  struct b {
    c: a
    d: UnsafePointer<Int8>
    let e = b(
      (0
    ""
