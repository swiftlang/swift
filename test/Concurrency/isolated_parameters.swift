// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -warn-concurrency
// REQUIRES: concurrency

@available(SwiftStdlib 5.5, *)
actor A {
  func f() { }
}

@available(SwiftStdlib 5.5, *)
extension Actor {
  func g() { }
}

@available(SwiftStdlib 5.5, *)
func testA<T: Actor>(
  a: isolated A,
  b: isolated T,
  c: isolated Int // expected-error{{'isolated' parameter has non-actor type 'Int'}}
) {
  a.f()
  a.g()
  b.g()
}

@available(SwiftStdlib 5.5, *)
typealias Fn = (isolated A) -> Void

@available(SwiftStdlib 5.5, *)
func globalFunc(_ a: A) { }

@available(SwiftStdlib 5.5, *)
func globalFuncIsolated(_: isolated A) { // expected-note{{calls to global function 'globalFuncIsolated' from outside of its actor context are implicitly asynchronous}}
  let _: Int = globalFuncIsolated // expected-error{{cannot convert value of type '(isolated A) -> ()' to specified type 'Int'}}
  let _: (A) -> Void = globalFuncIsolated // expected-error{{cannot convert value of type '(isolated A) -> ()' to specified type '(A) -> Void'}}
  let _: Fn = globalFunc // okay
}

@available(SwiftStdlib 5.5, *)
func testIsolatedParamCalls(a: isolated A, b: A) {
  globalFunc(a)
  globalFunc(b)

  globalFuncIsolated(a)
  globalFuncIsolated(b) // expected-error{{call to actor-isolated global function 'globalFuncIsolated' in a synchronous nonisolated context}}
}

@available(SwiftStdlib 5.5, *)
func testIsolatedParamCallsAsync(a: isolated A, b: A) async {
  globalFunc(a)
  globalFunc(b)

  globalFuncIsolated(a)
  globalFuncIsolated(b) // expected-error{{expression is 'async' but is not marked with 'await'}}
  // expected-note@-1{{calls to global function 'globalFuncIsolated' from outside of its actor context are implicitly asynchronous}}
  await globalFuncIsolated(b)
}
