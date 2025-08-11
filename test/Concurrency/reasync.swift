// RUN: %target-swift-frontend -enable-experimental-concurrency -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -enable-experimental-concurrency -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -enable-experimental-concurrency -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

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
  await reasyncFunction { } // expected-warning {{no 'async' operations occur within 'await' expression}}

  reasyncFunction { await asyncFunction() }
  // expected-error@-1:3 {{expression is 'async' but is not marked with 'await'}}{{3-3=await }}
  // expected-note@-2:3 {{call is 'async'}}

  await reasyncFunction { await asyncFunction() }
}

enum HorseError : Error {
  case colic
}

func callReasyncRethrowsFunction() async throws {
  reasyncRethrowsFunction { }
  await reasyncRethrowsFunction { }
  // expected-warning@-1 {{no 'async' operations occur within 'await' expression}}
  try reasyncRethrowsFunction { }
  // expected-warning@-1 {{no calls to throwing functions occur within 'try' expression}}
  try await reasyncRethrowsFunction { }
  // expected-warning@-1 {{no 'async' operations occur within 'await' expression}}
  // expected-warning@-2 {{no calls to throwing functions occur within 'try' expression}}

  reasyncRethrowsFunction { await asyncFunction() }
  // expected-error@-1:3 {{expression is 'async' but is not marked with 'await'}}{{3-3=await }}
  // expected-note@-2:3 {{call is 'async'}}

  await reasyncRethrowsFunction { await asyncFunction() }
  try reasyncRethrowsFunction { await asyncFunction() }
  // expected-warning@-1 {{no calls to throwing functions occur within 'try' expression}}
  // expected-error@-2:3 {{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  // expected-note@-3:7 {{call is 'async'}}

  try await reasyncRethrowsFunction { await asyncFunction() }
  // expected-warning@-1 {{no calls to throwing functions occur within 'try' expression}}

  reasyncRethrowsFunction { throw HorseError.colic }
  // expected-error@-1 {{call can throw but is not marked with 'try'}}
  // expected-note@-2 {{call is to 'rethrows' function, but argument function can throw}}
  await reasyncRethrowsFunction { throw HorseError.colic }
  // expected-error@-1 {{call can throw but is not marked with 'try'}}
  // expected-note@-2 {{call is to 'rethrows' function, but argument function can throw}}
  // expected-warning@-3 {{no 'async' operations occur within 'await' expression}}
  try reasyncRethrowsFunction { throw HorseError.colic }
  try await reasyncRethrowsFunction { throw HorseError.colic }
  // expected-warning@-1 {{no 'async' operations occur within 'await' expression}}

  reasyncRethrowsFunction { await asyncFunction(); throw HorseError.colic }
  // expected-error@-1 {{call can throw but is not marked with 'try'}}
  // expected-note@-2 {{call is to 'rethrows' function, but argument function can throw}}
  // expected-error@-3:3 {{expression is 'async' but is not marked with 'await'}}{{3-3=await }}
  // expected-note@-4:3 {{call is 'async'}}
  await reasyncRethrowsFunction { await asyncFunction(); throw HorseError.colic }
  // expected-error@-1 {{call can throw but is not marked with 'try'}}
  // expected-note@-2 {{call is to 'rethrows' function, but argument function can throw}}
  try reasyncRethrowsFunction { await asyncFunction(); throw HorseError.colic }
  // expected-error@-1:3 {{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  // expected-note@-2:7 {{call is 'async'}}
  try await reasyncRethrowsFunction { await asyncFunction(); throw HorseError.colic }
}

func computeValue() -> Int {}
func computeValueAsync() async -> Int {}

func reasyncWithAutoclosure(_: @autoclosure () async -> Int) reasync {}

func callReasyncWithAutoclosure1() {
// expected-note@-1 2{{add 'async' to function 'callReasyncWithAutoclosure1()' to make it asynchronous}}
  reasyncWithAutoclosure(computeValue())
  await reasyncWithAutoclosure(await computeValueAsync())
  // expected-error@-1 {{'async' call in a function that does not support concurrency}}

  await reasyncWithAutoclosure(computeValueAsync())
  // expected-error@-1:32 {{expression is 'async' but is not marked with 'await'}}{{32-32=await }}
  // expected-note@-2:32 {{call is 'async' in an autoclosure argument}}
  // expected-error@-3 {{'async' call in a function that does not support concurrency}}
}

func callReasyncWithAutoclosure2() async {
  reasyncWithAutoclosure(computeValue())
  await reasyncWithAutoclosure(await computeValueAsync())

  await reasyncWithAutoclosure(15 + computeValueAsync())
  // expected-error@-1:32 {{expression is 'async' but is not marked with 'await'}}{{32-32=await }}
  // expected-note@-2:37 {{call is 'async' in an autoclosure argument}}
}

//// Reasync body checking

// FIXME: Need tailored diagnostics to handle 'reasync' vs 'sync'.

func invalidReasyncBody(_: () async -> ()) reasync {
// expected-note@-1 {{add 'async' to function 'invalidReasyncBody' to make it asynchronous}}
  _ = await computeValueAsync()
  // expected-error@-1 {{'async' call in a function that does not support concurrency}}
}

func validReasyncBody(_ fn: () async -> ()) reasync {
  await fn()
}

//// String interpolation

func reasyncWithAutoclosure2(_: @autoclosure () async -> String) reasync {}

func callReasyncWithAutoclosure3() {
  let world = 123
  reasyncWithAutoclosure2("Hello \(world)")
}

//// async let

func callReasyncWithAutoclosure4(_: () async -> ()) reasync {
  await reasyncFunction {
    async let x = 123

    _ = await x
  }
}
