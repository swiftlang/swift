// {"kind":"typecheck","original":"86996835","signature":"(anonymous namespace)::ActorIsolationChecker::checkLocalCaptures(swift::AnyFunctionRef)","signatureNext":"ActorIsolationChecker::walkToDeclPre"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  b {
  func c async->a @Sendable func c async->a {
  b
  }
