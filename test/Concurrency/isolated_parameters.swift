// RUN: %target-typecheck-verify-swift  -disable-availability-checking -warn-concurrency
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

actor MyActor {
  func hello() {}
}

typealias MyFn = (isolated: Int) -> Void // expected-error {{function types cannot have argument labels; use '_' before 'isolated'}}
typealias MyFnFixed = (_: isolated MyActor) -> Void

func standalone(_: isolated MyActor) {}
func check() {
  let _: MyFnFixed = standalone
  let _: MyFnFixed = { (_: isolated MyActor) in () }
}


@available(SwiftStdlib 5.5, *)
protocol P {
  func f(isolated: MyActor) async
  func g(isolated x: MyActor) async
  func h(isolated MyActor: isolated MyActor)
  func i(isolated: isolated MyActor)
  func j(isolated: Int) -> Int
  func k(isolated y: Int) -> Int
  func l(isolated _: Int) -> Int
}

@available(SwiftStdlib 5.5, *)
struct S: P {
  func f(isolated: MyActor) async { await isolated.hello() }
  func g(isolated x: MyActor) async { await x.hello() }
  func h(isolated MyActor: isolated MyActor) { i(isolated: MyActor) }
  func i(isolated: isolated MyActor) { isolated.hello() }
  func j(isolated: Int) -> Int { return isolated }
  func k(isolated y: Int) -> Int { return j(isolated: y) }
  func l(isolated _: Int) -> Int { return k(isolated: 0) }
}


// Redeclaration checking
actor TestActor {
  func test() { // expected-note{{'test()' previously declared here}}
  }
  nonisolated func test() { // expected-error{{invalid redeclaration of 'test()'}}
  }
}

func redecl(_: TestActor) { } // expected-note{{'redecl' previously declared here}}
func redecl(_: isolated TestActor) { } // expected-error{{invalid redeclaration of 'redecl'}}
