// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

func reasyncFunction(_: () async -> ()) reasync {}

func reasyncRethrowsFunction(_: () async throws -> ()) reasync rethrows {}

func rethrowsReasyncFunction(_: () async throws -> ()) rethrows reasync {}
// expected-error@-1 {{'reasync' must precede 'rethrows'}}{{65-73=}}{{56-56=reasync }}

func asyncReasyncFunction(_: () async throws -> ()) async reasync {}
// expected-error@-1 {{'reasync' has already been specified}}{{59-67=}}

func reasyncParam(_: () reasync -> ()) {}
// expected-error@-1 {{only function declarations may be marked 'reasync'; did you mean 'async'?}}{{25-32=async}}

class Base {
  func reasyncMethod(_: () async -> ()) reasync {}
  // expected-note@-1 {{overridden declaration is here}}
}

class Derived : Base {
  override func reasyncMethod(_: () async -> ()) async {}
  // expected-error@-1 {{override of 'reasync' method should also be 'reasync'}}
}