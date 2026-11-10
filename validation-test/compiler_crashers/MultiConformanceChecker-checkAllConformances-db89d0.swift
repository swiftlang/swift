// {"kind":"typecheck","original":"702452ac","signature":"(anonymous namespace)::MultiConformanceChecker::checkAllConformances()","signatureAssert":"Assertion failed: (isa<SubscriptDecl>(Requirement) && \"Unhandled requirement kind\"), function getProtocolRequirementKind","signatureNext":"TypeChecker::checkConformancesInContext"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
class a: b  protocol b { macro a
