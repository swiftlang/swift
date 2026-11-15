// {"kind":"typecheck","original":"481e2c0e","signature":"(anonymous namespace)::ActorIsolationChecker::checkLocalCaptures(swift::AnyFunctionRef)","signatureAssert":"Assertion failed: (Captures.hasBeenComputed()), function getCaptureInfo","signatureNext":"ActorIsolationChecker::walkToExprPre"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func async {
  ...
  if #_hasSymbol()
    async let a = async
