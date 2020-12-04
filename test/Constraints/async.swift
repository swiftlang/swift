// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

// REQUIRES: concurrency

func doAsynchronously() async { }
func doSynchronously() { }

func testConversions() async {
  let _: () -> Void = doAsynchronously // expected-error{{invalid conversion from 'async' function of type '() async -> ()' to synchronous function type '() -> Void'}}
  let _: () async -> Void = doSynchronously // okay
}

// Overloading
@available(swift, deprecated: 4.0, message: "synchronous is no fun")
func overloadedSame(_: Int = 0) -> String { "synchronous" }

func overloadedSame() async -> String { "asynchronous" }

func overloaded() -> String { "synchronous" }
func overloaded() async -> Double { 3.14159 }

@available(swift, deprecated: 4.0, message: "synchronous is no fun")
func overloadedOptDifference() -> String { "synchronous" }

func overloadedOptDifference() async -> String? { nil }

func testOverloadedSync() {
  _ = overloadedSame() // expected-warning{{synchronous is no fun}}

  let _: String? = overloadedOptDifference() // expected-warning{{synchronous is no fun}}

  let _ = overloaded()
  let fn = {
    overloaded()
  }
  let _: Int = fn // expected-error{{value of type '() -> String'}}

  let fn2 = {
    print("fn2")
    _ = overloaded()
  }
  let _: Int = fn2 // expected-error{{value of type '() -> ()'}}

  let fn3 = {
    await overloaded()
  }
  let _: Int = fn3 // expected-error{{value of type '() async -> Double'}}

  let fn4 = {
    print("fn2")
    _ = await overloaded()
  }
  let _: Int = fn4 // expected-error{{value of type '() async -> ()'}}
}

func testOverloadedAsync() async {
  _ = await overloadedSame() // no warning

  let _: String? = await overloadedOptDifference() // no warning

  let _ = await overloaded()
  let _ = overloaded() // expected-error{{call is 'async' but is not marked with 'await'}}{{11-11=await }}

  let fn = {
    overloaded()
  }
  let _: Int = fn // expected-error{{value of type '() -> String'}}

  let fn2 = {
    print("fn2")
    _ = overloaded()
  }
  let _: Int = fn2 // expected-error{{value of type '() -> ()'}}

  let fn3 = {
    await overloaded()
  }
  let _: Int = fn3 // expected-error{{value of type '() async -> Double'}}

  let fn4 = {
    print("fn2")
    _ = await overloaded()
  }
  let _: Int = fn4 // expected-error{{value of type '() async -> ()'}}
}

func takesAsyncClosure(_ closure: () async -> String) -> Int { 0 }
func takesAsyncClosure(_ closure: () -> String) -> String { "" }

func testPassAsyncClosure() {
  let a = takesAsyncClosure { await overloadedSame() }
  let _: Double = a // expected-error{{convert value of type 'Int'}}

  let b = takesAsyncClosure { overloadedSame() } // expected-warning{{synchronous is no fun}}
  let _: Double = b // expected-error{{convert value of type 'String'}}
}

struct FunctionTypes {
  var syncNonThrowing: () -> Void
  var syncThrowing: () throws -> Void
  var asyncNonThrowing: () async -> Void
  var asyncThrowing: () async throws -> Void

  mutating func demonstrateConversions() {
    // Okay to add 'async' and/or 'throws'
    asyncNonThrowing = syncNonThrowing
    asyncThrowing = syncThrowing
    syncThrowing = syncNonThrowing
    asyncThrowing = asyncNonThrowing

    // Error to remove 'async' or 'throws'
    syncNonThrowing = asyncNonThrowing // expected-error{{invalid conversion}}
    syncThrowing = asyncThrowing       // expected-error{{invalid conversion}}
    syncNonThrowing = syncThrowing     // expected-error{{invalid conversion}}
    asyncNonThrowing = syncThrowing    // expected-error{{invalid conversion}}
  }
}
