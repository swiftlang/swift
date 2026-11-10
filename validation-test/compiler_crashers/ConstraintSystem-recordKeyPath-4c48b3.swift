// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::recordKeyPath(swift::KeyPathExpr const*, swift::TypeVariableType*, swift::TypeVariableType*, swift::DeclContext*)","signatureAssert":"Assertion failed: (inserted), function recordKeyPath","signatureNext":"ConstraintGenerator::visitKeyPathExpr"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a { b->a{switch self{case.c(\ d) e
