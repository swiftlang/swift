// RUN: %target-typecheck-verify-swift  -disable-availability-checking -warn-concurrency
// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
actor A {
  func f() { }
}

@available(SwiftStdlib 5.1, *)
extension Actor {
  func g() { }
}

@available(SwiftStdlib 5.1, *)
func testA<T: Actor>( // expected-error{{function cannot have more than one 'isolated' parameter ('a' and 'b')}}
  a: isolated A,
  b: isolated T,
  c: isolated Int // expected-error{{'isolated' parameter has non-actor type 'Int'}}
) {
  a.f()
  a.g()
  b.g()
}

actor Other {
  func inc() {}
  func pass(_ f: @Sendable  (Self) async -> Void) {}
}
actor Counter {
  func inc() {}
  func pass(_ f: @Sendable  (Self) async -> Void) {}
}

@available(SwiftStdlib 5.1, *)
func testDoubleIsolatedParams(a: isolated Counter, b: isolated Other) async { // expected-error{{function cannot have more than one 'isolated' parameter ('a' and 'b')}}
  a.inc()
  b.inc()
}

func taaaa() async {
  await Counter().pass { isoC in
    await Other().pass { isoO in
      await testDoubleIsolatedParams(a: isoC, b: isoO)
    }
  }
}

@available(SwiftStdlib 5.1, *)
typealias Fn = (isolated A) -> Void

@available(SwiftStdlib 5.1, *)
func globalFunc(_ a: A) { }

@available(SwiftStdlib 5.1, *)
func globalFuncIsolated(_: isolated A) { // expected-note{{calls to global function 'globalFuncIsolated' from outside of its actor context are implicitly asynchronous}}
  let _: Int = globalFuncIsolated // expected-error{{cannot convert value of type '(isolated A) -> ()' to specified type 'Int'}}
  let _: (A) -> Void = globalFuncIsolated // expected-error{{cannot convert value of type '(isolated A) -> ()' to specified type '(A) -> Void'}}
  let _: Fn = globalFunc // okay
}

@available(SwiftStdlib 5.1, *)
func testIsolatedParamCalls(a: isolated A, b: A) {
  globalFunc(a)
  globalFunc(b)

  globalFuncIsolated(a)
  globalFuncIsolated(b) // expected-error{{call to actor-isolated global function 'globalFuncIsolated' in a synchronous nonisolated context}}
}

@available(SwiftStdlib 5.1, *)
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


@available(SwiftStdlib 5.1, *)
protocol P {
  func f(isolated: MyActor) async
  func g(isolated x: MyActor) async
  func h(isolated MyActor: isolated MyActor)
  func i(isolated: isolated MyActor)
  func j(isolated: Int) -> Int
  func k(isolated y: Int) -> Int
  func l(isolated _: Int) -> Int
}

@available(SwiftStdlib 5.1, *)
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

func tuplify<Ts>(_ fn: (Ts) -> Void) {} // expected-note {{in call to function 'tuplify'}}

@available(SwiftStdlib 5.1, *)
func testTuplingIsolated(_ a: isolated A, _ b: isolated A) { // expected-error{{function cannot have more than one 'isolated' parameter ('a' and 'b')}}
  tuplify(testTuplingIsolated)
  // expected-error@-1 {{generic parameter 'Ts' could not be inferred}}
  // expected-error@-2 {{cannot convert value of type '(isolated A, isolated A) -> ()' to expected argument type '(Ts) -> Void'}}
}

// Inference of "isolated" on closure parameters.
@available(SwiftStdlib 5.1, *)
func testIsolatedClosureInference(a: A) {
  let _: (isolated A) -> Void = {
    $0.f()
  }

  let _: (isolated A) -> Void = {
    $0.f()
  }

  let _: (isolated A) -> Void = { a2 in
    a2.f()
  }

  let _: (isolated A) -> Void = { (a2: isolated A) in
    a2.f()
  }
}

// "isolated" existential parameters.
protocol P2: Actor {
  func m()
}

@available(SwiftStdlib 5.1, *)
func testExistentialIsolated(a: isolated P2, b: P2) async {
  a.m()
  await b.m()
  b.m() // expected-error{{expression is 'async' but is not marked with 'await'}}
  // expected-note@-1{{calls to instance method 'm()' from outside of its actor context are implicitly asynchronous}}
}

// "isolated" parameters of closures make the closure itself isolated.
extension TestActor {
  func isolatedMethod() { }
}

@available(SwiftStdlib 5.1, *)
func isolatedClosures() {
  let _: (isolated TestActor) -> Void = { (ta: isolated TestActor) in
    ta.isolatedMethod() // okay, isolated to ta

    _ = {
      ta.isolatedMethod() // okay, isolated to ta
    }
  }
}
