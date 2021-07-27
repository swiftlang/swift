// RUN: %target-swift-frontend -typecheck -verify %s  -verify-syntax-tree -disable-availability-checking

// REQUIRES: concurrency

func test1(asyncfp : () async -> Int, fp : () -> Int) async {
  _ = await asyncfp()
  _ = await asyncfp() + asyncfp()
  _ = await asyncfp() + fp()
  _ = await fp() + 42  // expected-warning {{no 'async' operations occur within 'await' expression}}
  _ = 32 + asyncfp() + asyncfp() // expected-error {{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  // expected-note@-1:12{{call is 'async'}}
  // expected-note@-2:24{{call is 'async'}}
}

func getInt() async -> Int { return 5 }

// Locations where "await" is prohibited.
func test2(
  defaulted: Int = await getInt() // expected-error{{'async' call cannot occur in a default argument}}
) async {
  defer {
    _ = await getInt() // expected-error{{'async' call cannot occur in a defer body}}
  }
  print("foo")
}

func test3() { // expected-note{{add 'async' to function 'test3()' to make it asynchronous}} {{13-13= async}}
  _ = await getInt() // expected-error{{'async' call in a function that does not support concurrency}}
}

func test4()throws { // expected-note{{add 'async' to function 'test4()' to make it asynchronous}} {{13-19=async throws}}
  _ = await getInt() // expected-error{{'async' call in a function that does not support concurrency}}
}

func test5<T>(_ f : () async throws -> T)  rethrows->T { // expected-note{{add 'async' to function 'test5' to make it asynchronous}} {{44-52=async rethrows}}
  return try await f() // expected-error{{'async' call in a function that does not support concurrency}}
}

enum SomeEnum: Int {
case foo = await 5 // expected-error{{raw value for enum case must be a literal}}
}

struct SomeStruct {
  var x = await getInt() // expected-error{{'async' call cannot occur in a property initializer}}
  static var y = await getInt() // expected-error{{'async' call cannot occur in a global variable initializer}}
}

func acceptAutoclosureNonAsync(_: @autoclosure () -> Int) async { }
func acceptAutoclosureAsync(_: @autoclosure () async -> Int) async { }
func acceptAutoclosureAsyncThrows(_: @autoclosure () async throws -> Int) async { }
func acceptAutoclosureAsyncThrowsRethrows(_: @autoclosure () async throws -> Int) async rethrows { }

func acceptAutoclosureNonAsyncBad(_: @autoclosure () async -> Int) -> Int { 0 }
// expected-error@-1{{'async' autoclosure parameter in a non-'async' function}}
// expected-note@-2{{add 'async' to function 'acceptAutoclosureNonAsyncBad' to make it asynchronous}} {{67-67= async}}

struct HasAsyncBad {
  init(_: @autoclosure () async -> Int) { }
  // expected-error@-1{{'async' autoclosure parameter in a non-'async' function}}
}

func testAutoclosure() async {
  await acceptAutoclosureAsync(await getInt())
  await acceptAutoclosureNonAsync(await getInt()) // expected-error{{'async' call in an autoclosure that does not support concurrency}}

  await acceptAutoclosureAsync(42 + getInt())
  // expected-error@-1:32{{expression is 'async' but is not marked with 'await'}}{{32-32=await }}
  // expected-note@-2:37{{call is 'async' in an autoclosure argument}}
  await acceptAutoclosureNonAsync(getInt()) // expected-error{{'async' call in an autoclosure that does not support concurrency}}
}

// Test inference of 'async' from the body of a closure.
func testClosure() {
  let closure = {
     await getInt()
  }

  let _: () -> Int = closure // expected-error{{invalid conversion from 'async' function of type '() async -> Int' to synchronous function type '() -> Int'}}

  let closure2 = { () async -> Int in
    print("here")
    return await getInt()
  }

  let _: () -> Int = closure2 // expected-error{{invalid conversion from 'async' function of type '() async -> Int' to synchronous function type '() -> Int'}}
}

// Nesting async and await together
func throwingAndAsync() async throws -> Int { return 0 }

enum HomeworkError : Error {
  case dogAteIt
}

func testThrowingAndAsync() async throws {
  _ = try await throwingAndAsync()
  _ = await try throwingAndAsync() // expected-warning{{'try' must precede 'await'}}{{7-13=}}{{17-17=await }}
  _ = await (try throwingAndAsync())
  _ = try (await throwingAndAsync())

  // Errors
  _ = await throwingAndAsync() // expected-error{{call can throw but is not marked with 'try'}}
  // expected-note@-1{{did you mean to use 'try'?}}{{7-7=try }}
  // expected-note@-2{{did you mean to handle error as optional value?}}{{7-7=try? }}
  // expected-note@-3{{did you mean to disable error propagation?}}{{7-7=try! }}
  _ = try throwingAndAsync()
  // expected-error@-1{{expression is 'async' but is not marked with 'await'}}{{11-11=await }}
  // expected-note@-2{{call is 'async'}}
}

func testExhaustiveDoCatch() async {
  do {
    _ = try await throwingAndAsync()
  } catch {
  }

  do {
    _ = try await throwingAndAsync()
    // expected-error@-1{{errors thrown from here are not handled because the enclosing catch is not exhaustive}}
  } catch let e as HomeworkError {
  }

  // Ensure that we infer 'async' through an exhaustive do-catch.
  let fn = {
    do {
      _ = try await throwingAndAsync()
    } catch {
    }
  }

  let _: Int = fn // expected-error{{cannot convert value of type '() async -> ()'}}

  // Ensure that we infer 'async' through a non-exhaustive do-catch.
  let fn2 = {
    do {
      _ = try await throwingAndAsync()
    } catch let e as HomeworkError {
    }
  }

  let _: Int = fn2 // expected-error{{cannot convert value of type '() async throws -> ()'}}
}

// String interpolation
func testStringInterpolation() async throws {
  // expected-error@+2:30{{expression is 'async' but is not marked with 'await'}}{{30-30=await }}
  // expected-note@+1:35{{call is 'async'}}
  _ = "Eventually produces \(32 + getInt())"
  _ = "Eventually produces \(await getInt())"
  _ = await "Eventually produces \(getInt())"
}

func invalidAsyncFunction() async {
  _ = try await throwingAndAsync() // expected-error {{errors thrown from here are not handled}}
}

func validAsyncFunction() async throws {
  _ = try await throwingAndAsync()
}

// Async let checking
func mightThrow() throws { }

func getIntUnsafely() throws -> Int { 0 }
func getIntUnsafelyAsync() async throws -> Int { 0 }

extension Error {
  var number: Int { 0 }
}

func testAsyncLet() async throws {
  async let x = await getInt()
  print(x) // expected-error{{expression is 'async' but is not marked with 'await'}}
  // expected-note@-1:9{{reference to async let 'x' is 'async'}}

  print(await x)

  do {
    try mightThrow()
  } catch let e where e.number == x { // expected-error{{async let 'x' cannot be referenced in a catch guard expression}}
  } catch {
  }

  async let x1 = getIntUnsafely() // okay, try is implicit here

  async let x2 = getInt() // okay, await is implicit here

  async let x3 = try getIntUnsafely()
  async let x4 = try! getIntUnsafely()
  async let x5 = try? getIntUnsafely()

  _ = await x1 // expected-error{{reading 'async let' can throw but is not marked with 'try'}}
  _ = await x2
  _ = try await x3
  _ = await x4
  _ = await x5
}

// expected-note@+1 4{{add 'async' to function 'testAsyncLetOutOfAsync()' to make it asynchronous}} {{30-30= async}}
func testAsyncLetOutOfAsync() {
  async let x = 1 // expected-error{{'async let' in a function that does not support concurrency}}
  // FIXME: expected-error@-1{{'async' call in a function that does not support concurrency}}

  _ = await x  // expected-error{{'async let' in a function that does not support concurrency}}
  _ = x // expected-error{{'async let' in a function that does not support concurrency}}
}
