// {"kind":"typecheck","original":"66e34f12","signature":"swift::constraints::ConstraintSystem::setTargetFor(swift::constraints::SyntacticElementTargetKey, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (inserted), function setTargetFor","signatureNext":"ConstraintSystem::resolveClosure"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a( [AnyHashable: Any])a([ {
