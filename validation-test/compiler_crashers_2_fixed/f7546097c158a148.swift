// {"kind":"typecheck","signature":"(anonymous namespace)::ActorIsolationChecker::checkLocalCaptures(swift::AnyFunctionRef)","signatureAssert":"Assertion failed: (Captures.hasBeenComputed()), function getCaptureInfo"}
// RUN: not %target-swift-frontend -typecheck %s
struct a {
  b : c =, d = {} protocol c
