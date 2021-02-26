// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

//// Basic definitions and parsing

func reasyncFunction(_: () async -> ()) reasync {}

func reasyncRethrowsFunction(_: () async throws -> ()) reasync rethrows {}

func rethrowsReasyncFunction(_: () async throws -> ()) rethrows reasync {}
// expected-error@-1 {{'reasync' must precede 'rethrows'}}{{65-73=}}{{56-56=reasync }}

func asyncReasyncFunction(_: () async throws -> ()) async reasync {}
// expected-error@-1 {{'reasync' has already been specified}}{{59-67=}}

func reasyncParam(_: () reasync -> ()) {}
// expected-error@-1 {{only function declarations may be marked 'reasync'; did you mean 'async'?}}{{25-32=async}}

//// Invalid cases

func noReasyncParams() reasync {}
// expected-error@-1 {{'reasync' function must take an 'async' function argument}}

//// Method override attribute checking

class Base {
  func reasyncMethod(_: () async -> ()) reasync {}
  // expected-note@-1 {{overridden declaration is here}}
}

class Derived : Base {
  override func reasyncMethod(_: () async -> ()) async {}
  // expected-error@-1 {{override of 'reasync' method should also be 'reasync'}}
}

//// Reasync call site checking

func asyncFunction() async {}

func callReasyncFunction() async {
  reasyncFunction { }
  await reasyncFunction { } // expected-warning {{no calls to 'async' functions occur within 'await' expression}}

  reasyncFunction { await asyncFunction() } // expected-error {{call is 'async' but is not marked with 'await'}}
  await reasyncFunction { await asyncFunction() }
}

enum HorseError : Error {
  case colic
}

func callReasyncRethrowsFunction() async throws {
  reasyncRethrowsFunction { }
  await reasyncRethrowsFunction { }
  // expected-warning@-1 {{no calls to 'async' functions occur within 'await' expression}}
  try reasyncRethrowsFunction { }
  // expected-warning@-1 {{no calls to throwing functions occur within 'try' expression}}
  try await reasyncRethrowsFunction { }
  // expected-warning@-1 {{no calls to 'async' functions occur within 'await' expression}}
  // expected-warning@-2 {{no calls to throwing functions occur within 'try' expression}}

  reasyncRethrowsFunction { await asyncFunction() }
  // expected-error@-1 {{call is 'async' but is not marked with 'await'}}
  await reasyncRethrowsFunction { await asyncFunction() }
  try reasyncRethrowsFunction { await asyncFunction() }
  // expected-error@-1 {{call is 'async' but is not marked with 'await'}}
  // expected-warning@-2 {{no calls to throwing functions occur within 'try' expression}}
  try await reasyncRethrowsFunction { await asyncFunction() }
  // expected-warning@-1 {{no calls to throwing functions occur within 'try' expression}}

  reasyncRethrowsFunction { throw HorseError.colic }
  // expected-error@-1 {{call can throw but is not marked with 'try'}}
  // expected-note@-2 {{call is to 'rethrows' function, but argument function can throw}}
  await reasyncRethrowsFunction { throw HorseError.colic }
  // expected-error@-1 {{call can throw but is not marked with 'try'}}
  // expected-note@-2 {{call is to 'rethrows' function, but argument function can throw}}
  // expected-warning@-3 {{no calls to 'async' functions occur within 'await' expression}}
  try reasyncRethrowsFunction { throw HorseError.colic }
  try await reasyncRethrowsFunction { throw HorseError.colic }
  // expected-warning@-1 {{no calls to 'async' functions occur within 'await' expression}}

  reasyncRethrowsFunction { await asyncFunction(); throw HorseError.colic }
  // expected-error@-1 {{call can throw but is not marked with 'try'}}
  // expected-note@-2 {{call is to 'rethrows' function, but argument function can throw}}
  // expected-error@-3 {{call is 'async' but is not marked with 'await'}}
  await reasyncRethrowsFunction { await asyncFunction(); throw HorseError.colic }
  // expected-error@-1 {{call can throw but is not marked with 'try'}}
  // expected-note@-2 {{call is to 'rethrows' function, but argument function can throw}}
  try reasyncRethrowsFunction { await asyncFunction(); throw HorseError.colic }
  // expected-error@-1 {{call is 'async' but is not marked with 'await'}}
  try await reasyncRethrowsFunction { await asyncFunction(); throw HorseError.colic }
}

func computeValue() -> Int {}
func computeValueAsync() async -> Int {}

func reasyncWithAutoclosure(_: @autoclosure () async -> Int) reasync {}

func callReasyncWithAutoclosure1() {
// expected-note@-1 2{{add 'async' to function 'callReasyncWithAutoclosure1()' to make it asynchronous}}
// expected-note@-2 2{{add '@asyncHandler' to function 'callReasyncWithAutoclosure1()' to create an implicit asynchronous context}}
  reasyncWithAutoclosure(computeValue())
  await reasyncWithAutoclosure(await computeValueAsync())
  // expected-error@-1 {{'async' in a function that does not support concurrency}}

  await reasyncWithAutoclosure(computeValueAsync())
  // expected-error@-1 {{call is 'async' in an autoclosure argument that is not marked with 'await'}}
  // expected-error@-2 {{'async' in a function that does not support concurrency}}
}

func callReasyncWithAutoclosure2() async {
  reasyncWithAutoclosure(computeValue())
  await reasyncWithAutoclosure(await computeValueAsync())

  await reasyncWithAutoclosure(computeValueAsync())
  // expected-error@-1 {{call is 'async' in an autoclosure argument that is not marked with 'await'}}
}
