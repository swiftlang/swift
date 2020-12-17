// RUN: %target-swift-frontend -typecheck -verify %s -enable-experimental-concurrency -verify-syntax-tree

// REQUIRES: concurrency

func test1(asyncfp : () async -> Int, fp : () -> Int) async {
  _ = await asyncfp()
  _ = await asyncfp() + asyncfp()
  _ = await asyncfp() + fp()
  _ = await fp() + 42  // expected-warning {{no calls to 'async' functions occur within 'await' expression}}
  _ = asyncfp() // expected-error {{call is 'async' but is not marked with 'await'}}{{7-7=await }}
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

func test3() { // expected-note{{add 'async' to function 'test3()' to make it asynchronous}} {{none}}
  // expected-note@-1{{add '@asyncHandler' to function 'test3()' to create an implicit asynchronous context}}{{1-1=@asyncHandler }}
  _ = await getInt() // expected-error{{'async' in a function that does not support concurrency}}
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
// expected-note@-2{{add 'async' to function 'acceptAutoclosureNonAsyncBad' to make it asynchronous}} {{none}}

struct HasAsyncBad {
  init(_: @autoclosure () async -> Int) { }
  // expected-error@-1{{'async' autoclosure parameter in a non-'async' function}}
}

func testAutoclosure() async {
  await acceptAutoclosureAsync(getInt()) // expected-error{{call is 'async' in an autoclosure argument that is not marked with 'await'}}{{32-32=await }}
  await acceptAutoclosureNonAsync(getInt()) // expected-error{{'async' in an autoclosure that does not support concurrency}}

  await acceptAutoclosureAsync(await getInt())
  await acceptAutoclosureNonAsync(await getInt()) // expected-error{{'async' in an autoclosure that does not support concurrency}}

  await acceptAutoclosureAsync(getInt()) // expected-error{{call is 'async' in an autoclosure argument that is not marked with 'await'}}{{32-32=await }}
  await acceptAutoclosureNonAsync(getInt()) // expected-error{{'async' in an autoclosure that does not support concurrency}}
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
  _ = await try throwingAndAsync()
  _ = try await throwingAndAsync() // expected-error{{'await' must precede 'try'}}{{11-17=}}{{7-7=await }}
  _ = await (try throwingAndAsync())
  _ = try (await throwingAndAsync())

  // Errors
  _ = await throwingAndAsync() // expected-error{{call can throw but is not marked with 'try'}}
  // expected-note@-1{{did you mean to use 'try'?}}
  // expected-note@-2{{did you mean to handle error as optional value?}}
  // expected-note@-3{{did you mean to disable error propagation?}}
  _ = try throwingAndAsync() // expected-error{{call is 'async' but is not marked with 'await'}}{{11-11=await }}
}

func testExhaustiveDoCatch() async {
  do {
    _ = await try throwingAndAsync()
  } catch {
  }

  do {
    _ = await try throwingAndAsync()
    // expected-error@-1{{errors thrown from here are not handled because the enclosing catch is not exhaustive}}
  } catch let e as HomeworkError {
  }

  // Ensure that we infer 'async' through an exhaustive do-catch.
  let fn = {
    do {
      _ = await try throwingAndAsync()
    } catch {
    }
  }

  let _: Int = fn // expected-error{{cannot convert value of type '() async -> ()'}}

  // Ensure that we infer 'async' through a non-exhaustive do-catch.
  let fn2 = {
    do {
      _ = await try throwingAndAsync()
    } catch let e as HomeworkError {
    }
  }

  let _: Int = fn2 // expected-error{{cannot convert value of type '() async throws -> ()'}}
}

// String interpolation
func testStringInterpolation() async throws {
  _ = "Eventually produces \(getInt())" // expected-error{{call is 'async' but is not marked with 'await'}}
  _ = "Eventually produces \(await getInt())"
  _ = await "Eventually produces \(getInt())"
}

func invalidAsyncFunction() async {
  _ = await try throwingAndAsync() // expected-error {{errors thrown from here are not handled}}
}

func validAsyncFunction() async throws {
  _ = await try throwingAndAsync()
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
  print(x) // expected-error{{reference to async let 'x' is not marked with 'await'}}
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
  _ = await try x3
  _ = await x4
  _ = await x5
}

// expected-note@+2 4{{add 'async' to function 'testAsyncLetOutOfAsync()' to make it asynchronous}} {{none}}
// expected-note@+1 4{{add '@asyncHandler' to function 'testAsyncLetOutOfAsync()' to create an implicit asynchronous context}} {{1-1=@asyncHandler }}
func testAsyncLetOutOfAsync() {
  async let x = 1 // expected-error{{'async let' in a function that does not support concurrency}}
  // FIXME: expected-error@-1{{'async' in a function that does not support concurrency}}

  _ = await x  // expected-error{{'async let' in a function that does not support concurrency}}
  _ = x // expected-error{{'async let' in a function that does not support concurrency}}
}
