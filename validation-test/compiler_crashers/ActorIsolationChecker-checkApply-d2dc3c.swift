// {"kind":"typecheck","original":"70f49ace","signature":"(anonymous namespace)::ActorIsolationChecker::checkApply(swift::ApplyExpr*)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast","signatureNext":"ActorIsolationChecker::walkToExprPre"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@MainActor class a
  deinit {
    a(
