// RUN: %target-typecheck-verify-swift  -disable-availability-checking
// REQUIRES: concurrency

actor SomeActor { }

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

@globalActor
struct OtherGlobalActor {
  static let shared = SomeActor()
}

func testConversions(f: @escaping @SomeGlobalActor (Int) -> Void, g: @escaping (Int) -> Void) {
  let _: Int = f // expected-error{{cannot convert value of type '@SomeGlobalActor (Int) -> Void' to specified type 'Int'}}

  let _: (Int) -> Void = f // expected-warning 2{{converting function value of type '@SomeGlobalActor (Int) -> Void' to '(Int) -> Void' loses global actor 'SomeGlobalActor'}}
  let _: @SomeGlobalActor (Int) -> Void = g // okay
  let _: @OtherGlobalActor (Int) -> Void = f // expected-error{{cannot convert value actor-isolated to 'SomeGlobalActor' to specified type actor-isolated to 'OtherGlobalActor'}}
}

@SomeGlobalActor func onSomeGlobalActor() -> Int { 5 }
@SomeGlobalActor(unsafe) func onSomeGlobalActorUnsafe() -> Int { 5 }

@OtherGlobalActor func onOtherGlobalActor() -> Int { 5 } // expected-note{{calls to global function 'onOtherGlobalActor()' from outside of its actor context are implicitly asynchronous}}
@OtherGlobalActor(unsafe) func onOtherGlobalActorUnsafe() -> Int { 5 }  // expected-note 2{{calls to global function 'onOtherGlobalActorUnsafe()' from outside of its actor context are implicitly asynchronous}}

func someSlowOperation() async -> Int { 5 }

func acceptOnSomeGlobalActor<T>(_: @SomeGlobalActor () -> T) { }

func testClosures(i: Int) async {
  // Global actors on synchronous closures become part of the type
  let cl1 = { @SomeGlobalActor in
    onSomeGlobalActor()
  }
  let _: Double = cl1 // expected-error{{cannot convert value of type '@SomeGlobalActor () -> Int' to specified type 'Double'}}

  // Global actors on async closures do not become part of the type
  let cl2 = { @SomeGlobalActor in
    await someSlowOperation()
  }
  let _: Double = cl2 // expected-error{{cannot convert value of type '() async -> Int' to specified type 'Double'}}

  let cl3 = { @SomeGlobalActor [i] in
    print(i + onSomeGlobalActor())
  }

  // okay to be explicit
  acceptOnSomeGlobalActor { @SomeGlobalActor in
    onSomeGlobalActor()
  }

  // Infer from context
  acceptOnSomeGlobalActor {
    onSomeGlobalActor()
  }

  acceptOnSomeGlobalActor { () -> Int in
    let i = onSomeGlobalActor()
    return i
  }

  acceptOnSomeGlobalActor { () -> Int in
    let i = onOtherGlobalActorUnsafe() // expected-error{{call to global actor 'OtherGlobalActor'-isolated global function 'onOtherGlobalActorUnsafe()' in a synchronous global actor 'SomeGlobalActor'-isolated context}}
    return i
  }
}

func testClosuresOld() {
  acceptOnSomeGlobalActor { () -> Int in
    let i = onSomeGlobalActor()
    return i
  }

  acceptOnSomeGlobalActor { () -> Int in
    let i = onSomeGlobalActorUnsafe()
    return i
  }

  acceptOnSomeGlobalActor { () -> Int in
    let i = onOtherGlobalActor() // expected-error{{call to global actor 'OtherGlobalActor'-isolated global function 'onOtherGlobalActor()' in a synchronous global actor 'SomeGlobalActor'-isolated context}}
    return i
  }

  acceptOnSomeGlobalActor { () -> Int in
    let i = onOtherGlobalActorUnsafe()
    return i
  }

  acceptOnSomeGlobalActor { @SomeGlobalActor () -> Int in
    let i = onOtherGlobalActorUnsafe() // expected-error{{call to global actor 'OtherGlobalActor'-isolated global function 'onOtherGlobalActorUnsafe()' in a synchronous global actor 'SomeGlobalActor'-isolated context}}
    return i
  }
}

// Test conversions that happen in various structural positions.
struct X<T> { } // expected-note{{arguments to generic parameter 'T' ('() -> Void' and '@SomeGlobalActor () -> Void') are expected to be equal}}

func f(_: (@SomeGlobalActor () -> Void)?) { }

func g(fn: (() -> Void)?) {
  f(fn)
}

func f2(_ x: X<@SomeGlobalActor () -> Void>) {
  g2(x) // expected-error{{converting function value of type '@SomeGlobalActor () -> Void' to '() -> Void' loses global actor 'SomeGlobalActor'}}
}
func g2(_ x: X<() -> Void>) {
  f2(x) // expected-error{{cannot convert value of type 'X<() -> Void>' to expected argument type 'X<@SomeGlobalActor () -> Void>'}}
}


func testTypesNonConcurrencyContext() { // expected-note{{add '@SomeGlobalActor' to make global function 'testTypesNonConcurrencyContext()' part of global actor 'SomeGlobalActor'}}
  let f1 = onSomeGlobalActor // expected-note{{calls to let 'f1' from outside of its actor context are implicitly asynchronous}}
  let f2 = onSomeGlobalActorUnsafe

  let _: () -> Int = f1 // expected-warning 2{{converting function value of type '@SomeGlobalActor () -> Int' to '() -> Int' loses global actor 'SomeGlobalActor'}}
  let _: () -> Int = f2

  _ = {
    let _: () -> Int = f1 // expected-warning 2{{converting function value of type '@SomeGlobalActor () -> Int' to '() -> Int' loses global actor 'SomeGlobalActor'}}
    let _: () -> Int = f2
  }

  @SomeGlobalActor func isolated() {
    let _: () -> Int = f1
    let _: () -> Int = f2
  }

  _ = f1() // expected-error{{call to global actor 'SomeGlobalActor'-isolated let 'f1' in a synchronous nonisolated context}}
  _ = f2()
}

func testTypesConcurrencyContext() async {
  let f1 = onSomeGlobalActor
  let f2 = onSomeGlobalActorUnsafe

  let _: () -> Int = f1 // expected-warning 2{{converting function value of type '@SomeGlobalActor () -> Int' to '() -> Int' loses global actor 'SomeGlobalActor'}}
  let _: () -> Int = f2 // expected-warning 2{{converting function value of type '@SomeGlobalActor () -> Int' to '() -> Int' loses global actor 'SomeGlobalActor'}}

  _ = {
    let _: () -> Int = f1 // expected-warning 2{{converting function value of type '@SomeGlobalActor () -> Int' to '() -> Int' loses global actor 'SomeGlobalActor'}}
    let _: () -> Int = f2 // expected-warning 2{{converting function value of type '@SomeGlobalActor () -> Int' to '() -> Int' loses global actor 'SomeGlobalActor'}}
  }

  let _: @SomeGlobalActor () -> () = {
    someGlobalActorFn()

    // NOTE: false warnings. this closure has the correct isolation.
    let _: () -> Int = f1 // expected-warning {{converting function value of type '@SomeGlobalActor () -> Int' to '() -> Int' loses global actor 'SomeGlobalActor'}}
    let _: () -> Int = f2 // expected-warning {{converting function value of type '@SomeGlobalActor () -> Int' to '() -> Int' loses global actor 'SomeGlobalActor'}}
  }

  _ = { @SomeGlobalActor in
    let _: () -> Int = f1
    let _: () -> Int = f2
  }

  // expected-error@+1{{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  _ = f1() //expected-note{{calls to let 'f1' from outside of its actor context are implicitly asynchronous}}
  // expected-error@+1{{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  _ = f2() //expected-note{{calls to let 'f2' from outside of its actor context are implicitly asynchronous}}

  // expected-error@+3{{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  //expected-note@+2 {{calls to let 'f1' from outside of its actor context are implicitly asynchronous}}
  // expected-note@+1 {{calls to let 'f2' from outside of its actor context are implicitly asynchronous}}
  _ = f1() + f2()

  _ = await f1()
  _ = await f2()
}

// Conversion from main-actor-qualified synchronous to unqualified asynchronous.
func test() {
  let _: () async -> Int = { @SomeGlobalActor in
    onSomeGlobalActor()
  }
}


// https://github.com/apple/swift/issues/61436
let x: @MainActor () -> Void
let y: () -> Void = {}
x = true ? y : y // Ok

func noActor(_ unit: () -> ()) { unit() }
@SomeGlobalActor func someGlobalActorFn() {}
@MainActor func mainActorFn() {}

@SomeGlobalActor func testDropActorInSameContext(_ f3: @SomeGlobalActor () -> (),
                                                 _ sendable: @escaping @Sendable @SomeGlobalActor () -> ()) {
  let f1 = onSomeGlobalActor
  let f2 = onSomeGlobalActorUnsafe

  let _: () -> Int = f1
  let _: () -> Int = f2
  noActor(f3)

  // ok if you drop both sendable and the actor
  noActor(sendable)
  let _: () -> () = sendable
  let _: @Sendable () -> () = sendable // expected-warning 2{{converting function value of type '@SomeGlobalActor @Sendable () -> ()' to '@Sendable () -> ()' loses global actor 'SomeGlobalActor'}}

  _ = {
    someGlobalActorFn()

    // FIXME: false warnings. this closure is not @Sendable so it has matching isolation.
    let _: () -> Int = f1 // expected-warning {{converting function value of type '@SomeGlobalActor () -> Int' to '() -> Int' loses global actor 'SomeGlobalActor'}}
    let _: () -> Int = f2 // expected-warning {{converting function value of type '@SomeGlobalActor () -> Int' to '() -> Int' loses global actor 'SomeGlobalActor'}}
  }

  _ = { @Sendable in
    let _: () -> Int = sendable // expected-warning 2{{converting function value of type '@SomeGlobalActor @Sendable () -> ()' to '() -> Int' loses global actor 'SomeGlobalActor'}}
  }
}

actor A {
  func illegal(_ stillIllegal: @MainActor () -> ()) {
    noActor(stillIllegal) // expected-warning 2{{converting function value of type '@MainActor () -> ()' to '() -> ()' loses global actor 'MainActor'}}
  }

  func sendAndDrop(_ g: @escaping @Sendable @MainActor () -> ()) async {
    _ = Task.detached { @MainActor in
      // FIXME: this is a false warning, probably from the constraint solver.
      noActor(g) // expected-warning {{converting function value of type '@MainActor @Sendable () -> ()' to '() -> ()' loses global actor 'MainActor'}}
    }

    _ = Task {
      // FIXME: this and many other warnings like this are emitted more than once, but are true warnings.
      noActor(g) // expected-warning 2{{converting function value of type '@MainActor @Sendable () -> ()' to '() -> ()' loses global actor 'MainActor'}}
    }

    _ = Task.detached {
      noActor(g) // expected-warning 2{{converting function value of type '@MainActor @Sendable () -> ()' to '() -> ()' loses global actor 'MainActor'}}
    }
  }
}

@MainActor func uhoh(_ f: @escaping @MainActor () -> (), _ sendableF: @escaping @Sendable @MainActor () -> ()) {

  let _: () async -> () = f
  let _: @Sendable () -> () = sendableF // expected-warning 2{{converting function value of type '@MainActor @Sendable () -> ()' to '@Sendable () -> ()' loses global actor 'MainActor'}}

  let _: () -> () = {
    noActor(f)
  }

  _ = { @Sendable in
    noActor(mainActorFn) // expected-warning {{converting function value of type '@MainActor () -> ()' to '() -> ()' loses global actor 'MainActor'}}
  }

  func local() {
    noActor(f)
    noActor(sendableF)
    let _: () -> () = mainActorFn
    let _: @Sendable () -> () = sendableF // expected-warning 2{{converting function value of type '@MainActor @Sendable () -> ()' to '@Sendable () -> ()' loses global actor 'MainActor'}}
  }

  _ = {
    func localNested() {
      noActor(f) // expected-warning 2{{converting function value of type '@MainActor () -> ()' to '() -> ()' loses global actor 'MainActor'}}
      noActor(sendableF) // expected-warning 2{{converting function value of type '@MainActor @Sendable () -> ()' to '() -> ()' loses global actor 'MainActor'}}
      let _: () -> () = mainActorFn // expected-warning 2{{converting function value of type '@MainActor () -> ()' to '() -> ()' loses global actor 'MainActor'}}
      let _: @Sendable () -> () = sendableF // expected-warning 2{{converting function value of type '@MainActor @Sendable () -> ()' to '@Sendable () -> ()' loses global actor 'MainActor'}}
    }
  }

  defer {
    noActor(f)
    noActor(sendableF)
    let _: () -> () = mainActorFn
    let _: @Sendable () -> () = sendableF // expected-warning 2{{converting function value of type '@MainActor @Sendable () -> ()' to '@Sendable () -> ()' loses global actor 'MainActor'}}
  }

  @Sendable func localSendable() {
    noActor(sendableF) // expected-warning 2{{converting function value of type '@MainActor @Sendable () -> ()' to '() -> ()' loses global actor 'MainActor'}}
    let _: () -> () = mainActorFn // expected-warning 2{{converting function value of type '@MainActor () -> ()' to '() -> ()' loses global actor 'MainActor'}}
  }

  let _: () -> () = {
    mainActorFn()  // FIXME: odd how this line alone causes the false warning below to be emitted.
    noActor(f)  // expected-warning {{converting function value of type '@MainActor () -> ()' to '() -> ()' loses global actor 'MainActor'}}
  }

  _ = {
    // FIXME: more false warnings
    noActor(f)  // expected-warning {{converting function value of type '@MainActor () -> ()' to '() -> ()' loses global actor 'MainActor'}}
    noActor(sendableF) // expected-warning {{converting function value of type '@MainActor @Sendable () -> ()' to '() -> ()' loses global actor 'MainActor'}}
  }

  let _: () -> () = {
    // FIXME: these are false warnings too.
    noActor(f) // expected-warning {{converting function value of type '@MainActor () -> ()' to '() -> ()' loses global actor 'MainActor'}}
    let _: () -> () = mainActorFn // expected-warning {{converting function value of type '@MainActor () -> ()' to '() -> ()' loses global actor 'MainActor'}}
  }

  let _: @SomeGlobalActor () -> () = {
    noActor(sendableF) // expected-warning 2{{converting function value of type '@MainActor @Sendable () -> ()' to '() -> ()' loses global actor 'MainActor'}}
    let _: () -> () = mainActorFn // expected-warning 2{{converting function value of type '@MainActor () -> ()' to '() -> ()' loses global actor 'MainActor'}}
  }
}

func stripActor(_ expr: @Sendable @autoclosure () -> (() -> ())) async {
  let f = expr()
  return f()
}

// NOTE: this warning is correct, but is only being caught by TypeCheckConcurrency's extra check.
@MainActor func exampleWhereConstraintSolverHasWrongDeclContext_v1() async {
  return await stripActor(mainActorFn) // expected-warning {{converting function value of type '@MainActor () -> ()' to '() -> ()' loses global actor 'MainActor'}}
}

// NOTE: this warning is correct, but is only being caught by TypeCheckConcurrency's extra check.
@MainActor func exampleWhereConstraintSolverHasWrongDeclContext_v2() async -> Int {
  async let a: () = noActor(mainActorFn) // expected-warning {{converting function value of type '@MainActor () -> ()' to '() -> ()' loses global actor 'MainActor'}}
  await a
}

@MainActor class MAIsolatedParent {}
class SubClass: MAIsolatedParent {
  func isoMethod() {
    let _: () -> () = mainActorFn
  }

  nonisolated func exemptMethod() {
    let _: () -> () = mainActorFn // expected-warning 2{{converting function value of type '@MainActor () -> ()' to '() -> ()' loses global actor 'MainActor'}}
  }
}

@MainActor func forceRemovalOfMainActor() {
  // expected to type-check as-is.
  let f: () -> () = (true ? mainActorFn : {})
  f()
}

@MainActor func forceAdditionOfMainActor() {
  // expected to type-check as-is.
  let f: @MainActor () -> () = (true ? mainActorFn : {})
  f()
}

// https://github.com/apple/swift/issues/62544
@globalActor
struct GA {
  actor A {}
  static let shared: A = A()
}

@globalActor
struct GAB {
  actor B {}
  static let shared: B = B()
}

func test_global_actor_mismatch() {
  let y: @GA () -> Void = {}
  let z: @GAB () -> Void = {}
  let x: @MainActor () -> Void  = y // expected-error{{cannot convert value actor-isolated to 'GA' to specified type actor-isolated to 'MainActor'}}
  let _ = y as @MainActor () -> Void // expected-error{{cannot convert value actor-isolated to 'GA' to function actor-isolated to 'MainActor' in coercion}}

  func f(_ fn: @GA () -> Void) {}
  f(x) // expected-error{{cannot convert value actor-isolated to 'MainActor' to expected argument type actor-isolated to 'GA'}}

  let _: [@MainActor () -> Void] = [y] // expected-error{{cannot convert value actor-isolated to 'GA' to expected element type actor-isolated to 'MainActor'}}
  let _: [@MainActor () -> Void] = [y, z] // expected-error{{cannot convert value actor-isolated to 'GA' to expected element type actor-isolated to 'MainActor'}}
  // expected-error@-1{{cannot convert value actor-isolated to 'GAB' to expected element type actor-isolated to 'MainActor'}}

  let _: [Int : @MainActor () -> Void] = [1: y] // expected-error{{cannot convert value actor-isolated to 'GA' to expected dictionary value type actor-isolated to 'MainActor'}}
  let _: [Int : @MainActor () -> Void] = [1: y, 2: z]  // expected-error{{cannot convert value actor-isolated to 'GA' to expected dictionary value type actor-isolated to 'MainActor'}}
  // expected-error@-1{{cannot convert value actor-isolated to 'GAB' to expected dictionary value type actor-isolated to 'MainActor'}}

  let _: () -> @MainActor () -> Void = {
    return y // expected-error{{cannot convert value actor-isolated to 'GA' to expected closure result type actor-isolated to 'MainActor'}}
  }

  let _: (@MainActor () -> Void, @GA () -> Void) = (y, y) // expected-error{{cannot convert type actor-isolated to 'GA' to type actor-isolated to 'MainActor' at tuple element '#0'}}
  let _: (@MainActor () -> Void, @MainActor () -> Void) = (y, z) // expected-error{{cannot convert type actor-isolated to 'GA' to type actor-isolated to 'MainActor' at tuple element '#0'}}
  // expected-error@-1{{cannot convert type actor-isolated to 'GAB' to type actor-isolated to 'MainActor' at tuple element '#1'}}

  f(true ? z : y) // expected-error{{result values in '? :' expression are functions isolated to different actors ('GAB' vs. 'GA')}}
  
  func g<T> ( _ fn: @escaping @GA () -> T) {
    let _: @MainActor () -> T = fn // expected-error{{cannot convert value actor-isolated to 'GA' to specified type actor-isolated to 'MainActor'}}
  }
}

struct GlobalType {}

@_Concurrency.MainActor
extension global_actor_function_types.GlobalType {
  @_Concurrency.MainActor static func ==(
    lhs: global_actor_function_types.GlobalType,
    rhs: global_actor_function_types.GlobalType
  ) -> Bool { true }
}
