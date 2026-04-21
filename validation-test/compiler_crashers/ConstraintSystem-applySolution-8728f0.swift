// {"kind":"typecheck","original":"961c3ba8","signature":"swift::constraints::ConstraintSystem::applySolution(swift::constraints::Solution&, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (OpenedExistentials.empty()), function finalize","signatureNext":"TypeChecker::typeCheckTarget"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b {
  struct c {
    subscript<d: Hashable>(d )  b
    let e = \c[b as! Hashable]
