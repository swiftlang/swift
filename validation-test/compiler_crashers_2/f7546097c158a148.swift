// {"signature":"(anonymous namespace)::ActorIsolationChecker::checkLocalCaptures(swift::AnyFunctionRef)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  b : c =, d = {} protocol c
