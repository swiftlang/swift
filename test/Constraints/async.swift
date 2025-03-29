// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency

@available(SwiftStdlib 5.5, *)
func doAsynchronously() async { }
@available(SwiftStdlib 5.5, *)
func doSynchronously() { }

@available(SwiftStdlib 5.5, *)
func testConversions() async {
  let _: () -> Void = doAsynchronously // expected-error{{invalid conversion from 'async' function of type '() async -> ()' to synchronous function type '() -> Void'}}
  let _: () async -> Void = doSynchronously // okay
}

// Overloading
@available(SwiftStdlib 5.5, *)
@available(swift, deprecated: 4.0, message: "synchronous is no fun")
func overloadedSame(_: Int = 0) -> String { "synchronous" }

@available(SwiftStdlib 5.5, *)
func overloadedSame() async -> String { "asynchronous" }

@available(SwiftStdlib 5.5, *)
func overloaded() -> String { "synchronous" }
@available(SwiftStdlib 5.5, *)
func overloaded() async -> Double { 3.14159 }

@available(SwiftStdlib 5.5, *)
@available(swift, deprecated: 4.0, message: "synchronous is no fun")
func overloadedOptDifference() -> String { "synchronous" }

@available(SwiftStdlib 5.5, *)
func overloadedOptDifference() async -> String? { nil }

@available(SwiftStdlib 5.5, *)
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

@available(SwiftStdlib 5.5, *)
func testOverloadedAsync() async {
  _ = await overloadedSame() // no warning

  let _: String? = await overloadedOptDifference() // no warning

  let _ = await overloaded()
  let _ = overloaded()
  // expected-error@-1:11{{expression is 'async' but is not marked with 'await'}}{{11-11=await }}
  // expected-note@-2:11{{call is 'async'}}


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

@available(SwiftStdlib 5.5, *)
func takesAsyncClosure(_ closure: () async -> String) -> Int { 0 }
@available(SwiftStdlib 5.5, *)
func takesAsyncClosure(_ closure: () -> String) -> String { "" }

@available(SwiftStdlib 5.5, *)
func testPassAsyncClosure() {
  let a = takesAsyncClosure { await overloadedSame() }
  let _: Double = a // expected-error{{convert value of type 'Int'}}

  let b = takesAsyncClosure { overloadedSame() } // expected-warning{{synchronous is no fun}}
  let _: Double = b // expected-error{{convert value of type 'String'}}
}

@available(SwiftStdlib 5.5, *)
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

// Overloading when there is conversion from sync to async.
@available(SwiftStdlib 5.5, *)
func bar(_ f: (Int) -> Int) -> Int {
  return f(2)
}

@available(SwiftStdlib 5.5, *)
func bar(_ f: (Int) async -> Int) async -> Int {
  return await f(2)
}

@available(SwiftStdlib 5.5, *)
func incrementSync(_ x: Int) -> Int {
  return x + 1
}

@available(SwiftStdlib 5.5, *)
func incrementAsync(_ x: Int) async -> Int {
  return x + 1
}

@available(SwiftStdlib 5.5, *)
func testAsyncWithConversions() async {
  _ = bar(incrementSync)
  _ = bar { -$0 }
  _ = bar(incrementAsync)
  // expected-error@-1:7{{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  // expected-note@-2:7{{call is 'async'}}
}

// rdar://88692889 - make sure overload resolution cues off the presence of
// 'await' in the body to determine whether to prefer async functions, not
// whether the closure is in a context where it will be converted to async.
@available(SwiftStdlib 5.1, *)
struct OverloadInImplicitAsyncClosure {
  init(int: Int) async throws {
    let task = Task { () -> Self in
      let result = try Self(int: int)
      return result
    }

    self = try await task.value
  }

  init(int: Int) throws { }
}

@available(SwiftStdlib 5.5, *)
func test(_: Int) async throws {}

@discardableResult
@available(SwiftStdlib 5.5, *)
func test(_: Int) -> String { "" }

@available(SwiftStdlib 5.5, *)
func compute(_: @escaping () -> Void) {}

@available(SwiftStdlib 5.5, *)
func test_sync_in_closure_context() {
  compute {
    test(42) // Ok (select sync overloads and discards the result)
  }
}

@available(SwiftStdlib 5.5, *)
func test_async_calls_in_async_context(v: Int) async {
  final class Test : Sendable {
    init(_: Int) {}
    init(_: Int) async {}

    func test(_: Int) {}
    func test(_: Int) async {}

    static func test(_: Int) {}
    static func test(_: Int) async {}
  }

  // Only implicit `.init` should be accepted with a warning due type-checker previously picking an incorrect overload.
  // FIXME: This should produce a warning once type-checker performance hacks are removed.
  _ = Test(v) // Temporary okay
  _ = Test.init(v) // expected-error {{expression is 'async' but is not marked with 'await'}} expected-note {{call is 'async'}}

  Test.test(v) // expected-error {{expression is 'async' but is not marked with 'await'}} expected-note {{call is 'async'}}
  Test(v).test(v) // expected-error {{expression is 'async' but is not marked with 'await'}} expected-note {{call is 'async'}}
}
