// {"kind":"typecheck","original":"85b6b48c","signature":"(anonymous namespace)::ActorIsolationChecker::checkApply(swift::ApplyExpr*)","signatureNext":"ActorIsolationChecker::walkToExprPre"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>(isolated: isolated Actor c: b
class d: Actor
  let c = (
  let e = d()
  await
    a(isolated: e c: c
