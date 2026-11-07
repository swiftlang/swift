// {"kind":"typecheck","original":"961c3ba8","signature":"swift::constraints::ConstraintSystem::applySolution(swift::constraints::Solution&, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (OpenedExistentials.empty()), function finalize","signatureNext":"TypeChecker::typeCheckTarget"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b {
  struct c {
    subscript<d: Hashable>(d )  b
    let e = \c[b as! Hashable]
