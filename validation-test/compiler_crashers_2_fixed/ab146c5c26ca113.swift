// {"signature":"getIsolationFromAttributes(swift::Decl const*, bool, bool)"}
// RUN: not %target-swift-frontend -typecheck %s
isolated let a
