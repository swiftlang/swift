// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::recordArgumentList(swift::constraints::ConstraintLocator*, swift::ArgumentList*)","signatureAssert":"Assertion failed: (inserted), function recordArgumentList","signatureNext":"ConstraintGenerator::visitApplyExpr"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol b let b = { (a : b)in switch a { case .c(d(
