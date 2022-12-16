// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserDiagnostics

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx
// REQUIRES: asserts

_ = [(Int) -> async throws Int]()
// expected-error@-1{{'async throws' may only occur before '->'}}
