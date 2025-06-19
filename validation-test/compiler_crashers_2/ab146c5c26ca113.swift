// {"signature":"getIsolationFromAttributes(swift::Decl const*, bool, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
isolated let a
