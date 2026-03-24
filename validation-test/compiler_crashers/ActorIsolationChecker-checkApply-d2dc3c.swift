// {"kind":"typecheck","original":"70f49ace","signature":"(anonymous namespace)::ActorIsolationChecker::checkApply(swift::ApplyExpr*)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast","signatureNext":"ActorIsolationChecker::walkToExprPre"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@MainActor class a
  deinit {
    a(
