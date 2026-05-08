// {"kind":"typecheck","original":"86996835","signature":"(anonymous namespace)::ActorIsolationChecker::checkLocalCaptures(swift::AnyFunctionRef)","signatureNext":"ActorIsolationChecker::walkToDeclPre"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  b {
  func c async->a @Sendable func c async->a {
  b
  }
