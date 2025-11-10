// {"kind":"typecheck","original":"b8967a13","signature":"(anonymous namespace)::ActorIsolationChecker::checkLocalCaptures(swift::AnyFunctionRef)","signatureAssert":"Assertion failed: (Captures.hasBeenComputed()), function getCaptureInfo"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a() {
  extension <#type#> {
    var b = {
    }
  }
}
