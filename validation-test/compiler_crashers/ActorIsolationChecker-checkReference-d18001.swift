// {"kind":"typecheck","languageMode":6,"original":"595f2199","signature":"(anonymous namespace)::ActorIsolationChecker::checkReference(swift::Expr*, swift::ConcreteDeclRef, swift::SourceLoc, std::__1::optional<(anonymous namespace)::PartialApplyThunkInfo>, swift::Expr*)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast","signatureNext":"ActorIsolationChecker::walkToExprPre"}
// RUN: not --crash %target-swift-frontend -typecheck -swift-version 6 %s
var a = 3
deinit {
  a
}
