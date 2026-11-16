// {"kind":"typecheck","original":"c6fc01a0","signature":"(anonymous namespace)::ActorIsolationChecker::checkApply(swift::ApplyExpr*)","signatureAssert":"Assertion failed: (!IsolationCrossing.has_value() && \"IsolationCrossing should not be set twice\"), function setIsolationCrossing","signatureNext":"ActorIsolationChecker::walkToExprPre"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  func
  a(Int = b(
@MainActor
  func b -> Int
