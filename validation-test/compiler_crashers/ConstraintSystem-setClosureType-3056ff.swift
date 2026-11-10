// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::setClosureType(swift::ClosureExpr const*, swift::FunctionType*)","signatureAssert":"Assertion failed: (inserted), function setClosureType","signatureNext":"ConstraintGenerator::visitClosureExpr"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol b let c = {(a : b)in switch a{case.d {
