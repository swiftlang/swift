// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::addFix(swift::constraints::ConstraintFix*)","signatureAssert":"Assertion failed: (inserted), function addFix","signatureNext":"ConstraintSystem::recordFix"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>(b) -> b a(["":0]) as [Int:String]
