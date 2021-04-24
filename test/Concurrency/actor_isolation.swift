// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -enable-experimental-async-handler -warn-concurrency
// REQUIRES: concurrency

let immutableGlobal: String = "hello"
var mutableGlobal: String = "can't touch this" // expected-note 5{{var declared here}}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func globalFunc() { }
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func acceptClosure<T>(_: () -> T) { }
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func acceptConcurrentClosure<T>(_: @Sendable () -> T) { }
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func acceptEscapingClosure<T>(_: @escaping () -> T) { }
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func acceptEscapingClosure<T>(_: @escaping (String) -> ()) async -> T? { nil }

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@discardableResult func acceptAsyncClosure<T>(_: () async -> T) -> T { }
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func acceptEscapingAsyncClosure<T>(_: @escaping () async -> T) { }
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func acceptInout<T>(_: inout T) {}


// ----------------------------------------------------------------------
// Actor state isolation restrictions
// ----------------------------------------------------------------------
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
actor MySuperActor {
  var superState: Int = 25 // expected-note {{mutation of this property is only permitted within the actor}}


  func superMethod() { // expected-note {{calls to instance method 'superMethod()' from outside of its actor context are implicitly asynchronous}}
    self.superState += 5
  }

  func superAsyncMethod() async { }

  subscript (index: Int) -> String {
    "\(index)"
  }
}

class Point {
  var x : Int = 0
  var y : Int = 0
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
actor MyActor: MySuperActor {
  nonisolated let immutable: Int = 17
  // expected-note@+2 2{{property declared here}}
  // expected-note@+1 6{{mutation of this property is only permitted within the actor}}
  var mutable: Int = 71

  // expected-note@+2 3 {{mutation of this property is only permitted within the actor}}
  // expected-note@+1 4{{property declared here}}
  var text: [String] = []

  nonisolated let point : Point = Point() // expected-error{{non-isolated let property 'point' has non-Sendable type 'Point'}}
  let otherPoint = Point()

  @MainActor
  var name : String = "koala" // expected-note{{property declared here}}

  func accessProp() -> String {
    return self.name // expected-error{{property 'name' isolated to global actor 'MainActor' can not be referenced from actor 'MyActor' in a synchronous context}}
  }

  class func synchronousClass() { }
  static func synchronousStatic() { }

  func synchronous() -> String { text.first ?? "nothing" } // expected-note 20{{calls to instance method 'synchronous()' from outside of its actor context are implicitly asynchronous}}
  func asynchronous() async -> String {
    super.superState += 4
    return synchronous()
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
actor Camera {
  func accessProp(act : MyActor) async -> String {
    return await act.name
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func checkAsyncPropertyAccess() async {
  let act = MyActor()
  let _ : Int = await act.mutable + act.mutable
  act.mutable += 1  // expected-error {{actor-isolated property 'mutable' can only be mutated from inside the actor}}

  act.superState += 1 // expected-error {{actor-isolated property 'superState' can only be mutated from inside the actor}}

  act.text[0].append("hello") // expected-error{{actor-isolated property 'text' can only be mutated from inside the actor}}

  // this is not the same as the above, because Array is a value type
  var arr = await act.text
  arr[0].append("hello")

  act.text.append("no") // expected-error{{actor-isolated property 'text' can only be mutated from inside the actor}}

  act.text[0] += "hello" // expected-error{{actor-isolated property 'text' can only be mutated from inside the actor}}

  _ = act.point
  _ = act.otherPoint
  // expected-error@-1{{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  // expected-note@-2{{property access is 'async'}}
  // expected-warning@-3{{cannot use property 'otherPoint' with a non-sendable type 'Point' across actors}}
  _ = await act.otherPoint // expected-warning{{cannot use property 'otherPoint' with a non-sendable type 'Point' across actors}}
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension MyActor {
  nonisolated var actorIndependentVar: Int {
    get { 5 }
    set { }
  }

  nonisolated func actorIndependentFunc(otherActor: MyActor) -> Int {
    _ = immutable
    _ = mutable // expected-error{{actor-isolated property 'mutable' can not be referenced from a non-isolated}}
    _ = text[0] // expected-error{{actor-isolated property 'text' can not be referenced from a non-isolated context}}
    _ = synchronous() // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from a non-isolated context}}

    // @actorIndependent
    _ = actorIndependentFunc(otherActor: self)
    _ = actorIndependentVar

    actorIndependentVar = 17
    _ = self.actorIndependentFunc(otherActor: self)
    _ = self.actorIndependentVar
    self.actorIndependentVar = 17

    // @actorIndependent on another actor
    _ = otherActor.actorIndependentFunc(otherActor: self)
    _ = otherActor.actorIndependentVar
    otherActor.actorIndependentVar = 17

    // async promotion
    _ = synchronous() // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from a non-isolated context}}

    // Global actors
    syncGlobalActorFunc() /// expected-error{{call to global actor 'SomeGlobalActor'-isolated global function 'syncGlobalActorFunc()' in a synchronous nonisolated context}}
    _ = syncGlobalActorFunc

    // Global data is okay if it is immutable.
    _ = immutableGlobal
    _ = mutableGlobal // expected-warning{{reference to var 'mutableGlobal' is not concurrency-safe because it involves shared mutable state}}

    // Partial application
    _ = synchronous  // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from a non-isolated context}}
    _ = super.superMethod // expected-error{{actor-isolated instance method 'superMethod()' can not be referenced from a non-isolated context}}
    acceptClosure(synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from a non-isolated context}}
    acceptClosure(self.synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from a non-isolated context}}
    acceptClosure(otherActor.synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can only be referenced on 'self'}}
    acceptEscapingClosure(synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from a non-isolated context}}}}
    acceptEscapingClosure(self.synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from a non-isolated}}
    acceptEscapingClosure(otherActor.synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can only be referenced on 'self'}}

    return 5
  }

  func testAsynchronous(otherActor: MyActor) async {
    _ = immutable
    _ = mutable
    mutable = 0
    _ = synchronous()
    _ = text[0]
    acceptInout(&mutable)

    // Accesses on 'self' are okay.
    _ = self.immutable
    _ = self.mutable
    self.mutable = 0
    _ = self.synchronous()
    _ = await self.asynchronous()
    _ = self.text[0]
    acceptInout(&self.mutable)
    _ = self[0]

    // Accesses on 'super' are okay.
    _ = super.superState
    super.superState = 0
    acceptInout(&super.superState)
    super.superMethod()
    await super.superAsyncMethod()
    _ = super[0]

    // Accesses on other actors can only reference immutable data synchronously,
    // otherwise the access is treated as async
    _ = otherActor.immutable // okay
    _ = otherActor.mutable  // expected-error{{expression is 'async' but is not marked with 'await'}}{{9-9=await }}
    // expected-note@-1{{property access is 'async'}}
    _ = await otherActor.mutable
    otherActor.mutable = 0  // expected-error{{actor-isolated property 'mutable' can only be mutated on 'self'}}
    acceptInout(&otherActor.mutable)  // expected-error{{actor-isolated property 'mutable' can only be used 'inout' on 'self'}}
    // expected-error@+2{{actor-isolated property 'mutable' can only be mutated on 'self'}}
    // expected-warning@+1{{no 'async' operations occur within 'await' expression}}
    await otherActor.mutable = 0

    _ = otherActor.synchronous()
    // expected-error@-1{{expression is 'async' but is not marked with 'await'}}{{9-9=await }}
    // expected-note@-2{{call is 'async'}}
    _ = await otherActor.asynchronous()
    _ = otherActor.text[0]
    // expected-error@-1{{expression is 'async' but is not marked with 'await'}}{{9-9=await }}
    // expected-note@-2{{property access is 'async'}}
    _ = await otherActor.text[0] // okay

    // Global data is okay if it is immutable.
    _ = immutableGlobal
    _ = mutableGlobal // expected-warning{{reference to var 'mutableGlobal' is not concurrency-safe because it involves shared mutable state}}

    // Global functions are not actually safe, but we allow them for now.
    globalFunc()

    // Class methods are okay.
    Self.synchronousClass()
    Self.synchronousStatic()

    // Global actors
    syncGlobalActorFunc() // expected-error{{expression is 'async' but is not marked with 'await'}}{{5-5=await }}
    // expected-note@-1{{call is 'async'}}

    await asyncGlobalActorFunc()

    // Closures.
    let localConstant = 17
    var localVar = 17

    // Non-escaping closures are okay.
    acceptClosure {
      _ = text[0]
      _ = self.synchronous()
      _ = localVar
      _ = localConstant
    }

    // Concurrent closures might run... concurrently.
    var otherLocalVar = 12
    acceptConcurrentClosure { [otherLocalVar] in
      defer {
        _ = otherLocalVar
      }

      _ = self.text[0] // expected-error{{actor-isolated property 'text' cannot be referenced from a concurrent closure}}
      _ = self.mutable // expected-error{{actor-isolated property 'mutable' cannot be referenced from a concurrent closure}}
      self.mutable = 0 // expected-error{{actor-isolated property 'mutable' cannot be mutated from a concurrent closure}}
      acceptInout(&self.mutable) // expected-error{{actor-isolated property 'mutable' cannot be used 'inout' from a concurrent closure}}
      _ = self.immutable
      _ = self.synchronous() // expected-error{{actor-isolated instance method 'synchronous()' cannot be referenced from a concurrent closure}}
      _ = localVar // expected-error{{reference to captured var 'localVar' in concurrently-executing code}}
      localVar = 25 // expected-error{{mutation of captured var 'localVar' in concurrently-executing code}}
      _ = localConstant

      _ = otherLocalVar
    }
    otherLocalVar = 17

    acceptConcurrentClosure { [weak self, otherLocalVar] in
      defer {
        _ = self?.actorIndependentVar
      }

      _ = otherLocalVar
    }

    // Escaping closures are still actor-isolated
    acceptEscapingClosure {
      _ = self.text[0]
      _ = self.mutable
      self.mutable = 0
      acceptInout(&self.mutable)
      _ = self.immutable
      _ = self.synchronous()
      _ = localVar
      _ = localConstant
    }

    // Local functions might run concurrently.
    @Sendable func localFn1() {
      _ = self.text[0] // expected-error{{actor-isolated property 'text' cannot be referenced from a concurrent function}}
      _ = self.synchronous() // expected-error{{actor-isolated instance method 'synchronous()' cannot be referenced from a concurrent function}}
      _ = localVar // expected-error{{reference to captured var 'localVar' in concurrently-executing code}}
      localVar = 25 // expected-error{{mutation of captured var 'localVar' in concurrently-executing code}}
      _ = localConstant
    }

    @Sendable func localFn2() {
      acceptClosure {
        _ = text[0]  // expected-error{{actor-isolated property 'text' cannot be referenced from a concurrent function}}
        _ = self.synchronous() // expected-error{{actor-isolated instance method 'synchronous()' cannot be referenced from a concurrent function}}
        _ = localVar // expected-error{{reference to captured var 'localVar' in concurrently-executing code}}
        localVar = 25 // expected-error{{mutation of captured var 'localVar' in concurrently-executing code}}
        _ = localConstant
      }
    }

    acceptEscapingClosure {
      localFn1()
      localFn2()
    }

    localVar = 0

    // Partial application
    _ = synchronous  // expected-error{{actor-isolated instance method 'synchronous()' can not be partially applied}}
    _ = super.superMethod
    acceptClosure(synchronous)
    acceptClosure(self.synchronous)
    acceptClosure(otherActor.synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can only be referenced on 'self'}}
    acceptEscapingClosure(synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can not be partially applied}}
    acceptEscapingClosure(self.synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can not be partially applied}}
    acceptEscapingClosure(otherActor.synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can only be referenced on 'self'}}

    acceptAsyncClosure(self.asynchronous)
    acceptEscapingAsyncClosure(self.asynchronous)
  }
}

// ----------------------------------------------------------------------
// Global actor isolation restrictions
// ----------------------------------------------------------------------
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
actor SomeActor { }

@globalActor
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
struct SomeGlobalActor {
  static let shared = SomeActor()
}

@globalActor
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
struct SomeOtherGlobalActor {
  static let shared = SomeActor()
}

@globalActor
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
struct GenericGlobalActor<T> {
  static var shared: SomeActor { SomeActor() }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@SomeGlobalActor func onions() {} // expected-note{{calls to global function 'onions()' from outside of its actor context are implicitly asynchronous}}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@MainActor func beets() { onions() } // expected-error{{call to global actor 'SomeGlobalActor'-isolated global function 'onions()' in a synchronous main actor-isolated context}}
// expected-note@-1{{calls to global function 'beets()' from outside of its actor context are implicitly asynchronous}}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
actor Crystal {
  // expected-note@+2 {{property declared here}}
  // expected-note@+1 2 {{mutation of this property is only permitted within the actor}}
  @SomeGlobalActor var globActorVar : Int = 0

  // expected-note@+1 {{mutation of this property is only permitted within the actor}}
  @SomeGlobalActor var globActorProp : Int {
    get { return 0 }
    set {}
  }

  @SomeGlobalActor func foo(_ x : inout Int) {}

  func referToGlobProps() async {
    _ = await globActorVar + globActorProp

    globActorProp = 20 // expected-error {{property 'globActorProp' isolated to global actor 'SomeGlobalActor' can not be mutated from actor 'Crystal'}}

    globActorVar = 30 // expected-error {{property 'globActorVar' isolated to global actor 'SomeGlobalActor' can not be mutated from actor 'Crystal'}}

    // expected-error@+2 {{property 'globActorVar' isolated to global actor 'SomeGlobalActor' can not be used 'inout' from actor 'Crystal'}}
    // expected-error@+1 {{actor-isolated property 'globActorVar' cannot be passed 'inout' to implicitly 'async' function call}}
    await self.foo(&globActorVar)

    _ = self.foo
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@SomeGlobalActor func syncGlobalActorFunc() { syncGlobalActorFunc() } // expected-note {{calls to global function 'syncGlobalActorFunc()' from outside of its actor context are implicitly asynchronous}}
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@SomeGlobalActor func asyncGlobalActorFunc() async { await asyncGlobalActorFunc() }

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@SomeOtherGlobalActor func syncOtherGlobalActorFunc() { }

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@SomeOtherGlobalActor func asyncOtherGlobalActorFunc() async {
  await syncGlobalActorFunc()
  await asyncGlobalActorFunc()
}

// test global actor funcs that are marked asyncHandler
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@SomeGlobalActor func goo1() async {
  let _ = goo2
  // expected-error@+1{{expression is 'async' but is not marked with 'await'}}{{3-3=await }}
  goo2() // expected-note{{call is 'async'}}
}
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@asyncHandler @SomeOtherGlobalActor func goo2() { await goo1() }

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func testGlobalActorClosures() {
  let _: Int = acceptAsyncClosure { @SomeGlobalActor in
    syncGlobalActorFunc()
    syncOtherGlobalActorFunc() // expected-error{{expression is 'async' but is not marked with 'await'}}{{5-5=await }}
    // expected-note@-1{{call is 'async'}}

    await syncOtherGlobalActorFunc()
    return 17
  }

  acceptConcurrentClosure { @SomeGlobalActor in 5 } // expected-error{{converting function value of type '@SomeGlobalActor @Sendable () -> Int' to '@Sendable () -> Int' loses global actor 'SomeGlobalActor'}}
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension MyActor {
  @SomeGlobalActor func onGlobalActor(otherActor: MyActor) async {
    // Access to other functions in this actor are okay.
    syncGlobalActorFunc()
    await asyncGlobalActorFunc()

    // Other global actors are ok if marked with 'await'
    await syncOtherGlobalActorFunc()
    await asyncOtherGlobalActorFunc()

    _ = immutable
    _ = mutable // expected-error{{expression is 'async' but is not marked with 'await'}}{{9-9=await }}
    // expected-note@-1{{property access is 'async'}}
    _ = await mutable
    _ = synchronous() // expected-error{{expression is 'async' but is not marked with 'await'}}{{9-9=await }}
    // expected-note@-1{{call is 'async'}}
    _ = await synchronous()
    _ = text[0] // expected-error{{expression is 'async' but is not marked with 'await'}}
    // expected-note@-1{{property access is 'async'}}

    _ = await text[0]

    // Accesses on 'self' are only okay for immutable and asynchronous, because
    // we are outside of the actor instance.
    _ = self.immutable
    _ = self.synchronous() // expected-error{{expression is 'async' but is not marked with 'await'}}{{9-9=await }}
    // expected-note@-1{{call is 'async'}}
    _ = await self.synchronous()

    _ = await self.asynchronous()
    _ = self.text[0] // expected-error{{expression is 'async' but is not marked with 'await'}}{{9-9=await }}
    // expected-note@-1{{property access is 'async'}}
    _ = self[0] // expected-error{{expression is 'async' but is not marked with 'await'}}{{9-9=await }}
    // expected-note@-1{{subscript access is 'async'}}
    _ = await self.text[0]
    _ = await self[0]

    // Accesses on 'super' are not okay without 'await'; we're outside of the actor.
    _ = super.superState // expected-error{{expression is 'async' but is not marked with 'await'}}{{9-9=await }}
    // expected-note@-1{{property access is 'async'}}
    _ = await super.superState
    super.superMethod() // expected-error{{expression is 'async' but is not marked with 'await'}}{{5-5=await }}
  // expected-note@-1{{call is 'async'}}

    await super.superMethod()
    await super.superAsyncMethod()
    _ = super[0] // expected-error{{expression is 'async' but is not marked with 'await'}}{{9-9=await }}
    // expected-note@-1{{subscript access is 'async'}}
    _ = await super[0]

    // Accesses on other actors can only reference immutable data or
    // call asychronous methods
    _ = otherActor.immutable // okay
    _ = otherActor.synchronous() // expected-error{{expression is 'async' but is not marked with 'await'}}{{9-9=await }}
    // expected-note@-1{{call is 'async'}}
    _ = otherActor.synchronous  // expected-error{{actor-isolated instance method 'synchronous()' can only be referenced on 'self'}}
    _ = await otherActor.asynchronous()
    _ = otherActor.text[0] // expected-error{{expression is 'async' but is not marked with 'await'}}{{9-9=await }}
    // expected-note@-1{{property access is 'async'}}
    _ = await otherActor.text[0]
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
struct GenericStruct<T> {
  @GenericGlobalActor<T> func f() { } // expected-note {{calls to instance method 'f()' from outside of its actor context are implicitly asynchronous}}

  @GenericGlobalActor<T> func g() {
    f() // okay
  }

  @GenericGlobalActor<String> func h() {
    f() // expected-error{{call to global actor 'GenericGlobalActor<T>'-isolated instance method 'f()' in a synchronous global actor 'GenericGlobalActor<String>'-isolated context}}
    let fn = f // expected-note{{calls to let 'fn' from outside of its actor context are implicitly asynchronous}}
    fn() // expected-error{{call to global actor 'GenericGlobalActor<T>'-isolated let 'fn' in a synchronous global actor 'GenericGlobalActor<String>'-isolated context}}
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension GenericStruct where T == String {
  @GenericGlobalActor<T>
  func h2() {
    f()
    g()
    h()
  }
}

@SomeGlobalActor
var number: Int = 42 // expected-note {{var declared here}}

// expected-note@+1 {{add '@SomeGlobalActor' to make global function 'badNumberUser()' part of global actor 'SomeGlobalActor'}}
func badNumberUser() {
  //expected-error@+1{{var 'number' isolated to global actor 'SomeGlobalActor' can not be referenced from this synchronous context}}
  print("The protected number is: \(number)")
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func asyncBadNumberUser() async {
  print("The protected number is: \(await number)")
}

// ----------------------------------------------------------------------
// Non-actor code isolation restrictions
// ----------------------------------------------------------------------
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func testGlobalRestrictions(actor: MyActor) async {
  let _ = MyActor()

  // references to sync methods must be fully applied.
  _ = actor.synchronous // expected-error{{actor-isolated instance method 'synchronous()' can only be referenced from inside the actor}}
  _ = actor.asynchronous

  // any kind of method can be called from outside the actor, so long as it's marked with 'await'
  _ = actor.synchronous() // expected-error{{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  // expected-note@-1{{call is 'async}}
  _ = actor.asynchronous() // expected-error{{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  // expected-note@-1{{call is 'async'}}

  _ = await actor.synchronous()
  _ = await actor.asynchronous()

  // stored and computed properties can be accessed. Only immutable stored properties can be accessed without 'await'
  _ = actor.immutable
  _ = await actor.immutable // expected-warning {{no 'async' operations occur within 'await' expression}}
  _ = actor.mutable  // expected-error{{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  // expected-note@-1{{property access is 'async'}}
  _ = await actor.mutable
  _ = actor.text[0] // expected-error{{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  // expected-note@-1{{property access is 'async'}}
  _ = await actor.text[0]
  _ = actor[0] // expected-error{{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  // expected-note@-1{{subscript access is 'async'}}
  _ = await actor[0]

  // @actorIndependent declarations are permitted.
  _ = actor.actorIndependentFunc(otherActor: actor)
  _ = actor.actorIndependentVar
  actor.actorIndependentVar = 5

  // Operations on non-instances are permitted.
  MyActor.synchronousStatic()
  MyActor.synchronousClass()

  // Global mutable state cannot be accessed.
  _ = mutableGlobal // expected-warning{{reference to var 'mutableGlobal' is not concurrency-safe because it involves shared mutable state}}

  // Local mutable variables cannot be accessed from concurrently-executing
  // code.
  var i = 17
  acceptConcurrentClosure {
    _ = i // expected-error{{reference to captured var 'i' in concurrently-executing code}}
    i = 42 // expected-error{{mutation of captured var 'i' in concurrently-executing code}}
  }
  print(i)

  acceptConcurrentClosure { [i] in
    _ = i
  }

  print("\(number)") //expected-error {{expression is 'async' but is not marked with 'await'}}{{12-12=await }}
  //expected-note@-1{{property access is 'async'}}

}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func f() {
  acceptConcurrentClosure {
    _ = mutableGlobal // expected-warning{{reference to var 'mutableGlobal' is not concurrency-safe because it involves shared mutable state}}
  }

  @Sendable func g() {
    _ = mutableGlobal // expected-warning{{reference to var 'mutableGlobal' is not concurrency-safe because it involves shared mutable state}}
  }
}

// ----------------------------------------------------------------------
// Local function isolation restrictions
// ----------------------------------------------------------------------
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func checkLocalFunctions() async {
  var i = 0
  var j = 0

  func local1() {
    i = 17
  }

  func local2() { // expected-error{{concurrently-executed local function 'local2()' must be marked as '@Sendable'}}{{3-3=@Sendable }}
    j = 42
  }

  // Okay to call locally.
  local1()
  local2()

  // Non-concurrent closures don't cause problems.
  acceptClosure {
    local1()
    local2()
  }

  // Escaping closures can make the local function execute concurrently.
  acceptConcurrentClosure {
    local2()
  }

  print(i)
  print(j)

  var k = 17
  func local4() {
    acceptConcurrentClosure {
      local3()
    }
  }

  func local3() { // expected-error{{concurrently-executed local function 'local3()' must be marked as '@Sendable'}}
    k = 25 // expected-error{{mutation of captured var 'k' in concurrently-executing code}}
  }

  print(k)
}

// ----------------------------------------------------------------------
// Lazy properties with initializers referencing 'self'
// ----------------------------------------------------------------------

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
actor LazyActor {
    var v: Int = 0
    // expected-note@-1 6 {{property declared here}}

    nonisolated let l: Int = 0

    lazy var l11: Int = { v }()
    lazy var l12: Int = v
    lazy var l13: Int = { self.v }()
    lazy var l14: Int = self.v
    lazy var l15: Int = { [unowned self] in self.v }() // expected-error{{actor-isolated property 'v' can not be referenced from a non-isolated context}}

    lazy var l21: Int = { l }()
    lazy var l22: Int = l
    lazy var l23: Int = { self.l }()
    lazy var l24: Int = self.l
    lazy var l25: Int = { [unowned self] in self.l }()

    nonisolated lazy var l31: Int = { v }()
    // expected-error@-1 {{actor-isolated property 'v' can not be referenced from a non-isolated context}}
    nonisolated lazy var l32: Int = v
    // expected-error@-1 {{actor-isolated property 'v' can not be referenced from a non-isolated context}}
    nonisolated lazy var l33: Int = { self.v }()
    // expected-error@-1 {{actor-isolated property 'v' can not be referenced from a non-isolated context}}
    nonisolated lazy var l34: Int = self.v
    // expected-error@-1 {{actor-isolated property 'v' can not be referenced from a non-isolated context}}
    nonisolated lazy var l35: Int = { [unowned self] in self.v }()
    // expected-error@-1 {{actor-isolated property 'v' can not be referenced from a non-isolated context}}

    nonisolated lazy var l41: Int = { l }()
    nonisolated lazy var l42: Int = l
    nonisolated lazy var l43: Int = { self.l }()
    nonisolated lazy var l44: Int = self.l
    nonisolated lazy var l45: Int = { [unowned self] in self.l }()
}

// Infer global actors from context only for instance members.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@MainActor
class SomeClassInActor {
  enum ID: String { case best }

  func inActor() { } // expected-note{{calls to instance method 'inActor()' from outside of its actor context are implicitly asynchronous}}
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension SomeClassInActor.ID {
  func f(_ object: SomeClassInActor) { // expected-note{{add '@MainActor' to make instance method 'f' part of global actor 'MainActor'}}
    object.inActor() // expected-error{{call to main actor-isolated instance method 'inActor()' in a synchronous nonisolated context}}
  }
}

// ----------------------------------------------------------------------
// Initializers
// ----------------------------------------------------------------------
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
actor SomeActorWithInits {
  var mutableState: Int = 17
  var otherMutableState: Int

  init() {
    self.mutableState = 42
    self.otherMutableState = 17

    self.isolated()
  }

  func isolated() { }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@MainActor
class SomeClassWithInits {
  var mutableState: Int = 17
  var otherMutableState: Int

  static var shared = SomeClassWithInits() // expected-note 2{{static property declared here}}

  init() { // expected-note{{calls to initializer 'init()' from outside of its actor context are implicitly asynchronous}}
    self.mutableState = 42
    self.otherMutableState = 17

    self.isolated()
  }

  deinit {
    print(mutableState) // Okay, we're actor-isolated
    print(SomeClassWithInits.shared) // expected-error{{static property 'shared' isolated to global actor 'MainActor' can not be referenced from this synchronous context}}
    beets() //expected-error{{call to main actor-isolated global function 'beets()' in a synchronous nonisolated context}}
  }

  func isolated() { }

  static func staticIsolated() { // expected-note{{calls to static method 'staticIsolated()' from outside of its actor context are implicitly asynchronous}}
    _ = SomeClassWithInits.shared
  }

  func hasDetached() {
    detach {
      // okay
      await self.isolated() // expected-warning{{cannot use parameter 'self' with a non-sendable type 'SomeClassWithInits' from concurrently-executed code}}
      self.isolated() // expected-warning{{cannot use parameter 'self' with a non-sendable type 'SomeClassWithInits' from concurrently-executed code}}
      // expected-error@-1{{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
      // expected-note@-2{{call is 'async'}}

      print(await self.mutableState) // expected-warning{{cannot use parameter 'self' with a non-sendable type 'SomeClassWithInits' from concurrently-executed code}}
    }
  }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func outsideSomeClassWithInits() { // expected-note 3 {{add '@MainActor' to make global function 'outsideSomeClassWithInits()' part of global actor 'MainActor'}}
  _ = SomeClassWithInits() // expected-error{{call to main actor-isolated initializer 'init()' in a synchronous nonisolated context}}
  _ = SomeClassWithInits.shared // expected-error{{static property 'shared' isolated to global actor 'MainActor' can not be referenced from this synchronous context}}
  SomeClassWithInits.staticIsolated() // expected-error{{call to main actor-isolated static method 'staticIsolated()' in a synchronous nonisolated context}}
}

// ----------------------------------------------------------------------
// Actor protocols.
// ----------------------------------------------------------------------
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
protocol P: Actor {
  func f()
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension P {
  func g() { f() }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
actor MyActorP: P {
  func f() { }

  func h() { g() }
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func testCrossActorProtocol<T: P>(t: T) async {
  await t.f()
  await t.g()
  t.f()
  // expected-error@-1{{expression is 'async' but is not marked with 'await'}}{{3-3=await }}
  // expected-note@-2{{call is 'async'}}
  t.g()
  // expected-error@-1{{expression is 'async' but is not marked with 'await'}}{{3-3=await }}
  // expected-note@-2{{call is 'async'}}
}

// ----------------------------------------------------------------------
// @_inheritActorContext
// ----------------------------------------------------------------------
func acceptAsyncSendableClosure<T>(_: @Sendable () async -> T) { }
func acceptAsyncSendableClosureInheriting<T>(@_inheritActorContext _: @Sendable () async -> T) { }

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension MyActor {
  func testSendableAndInheriting() {
    acceptAsyncSendableClosure {
      synchronous() // expected-error{{actor-isolated instance method 'synchronous()' cannot be referenced from a concurrent closure}}
    }

    acceptAsyncSendableClosure {
      await synchronous() // ok
    }

    acceptAsyncSendableClosureInheriting {
      synchronous() // okay
    }

    acceptAsyncSendableClosureInheriting {
      await synchronous() // expected-warning{{no 'async' operations occur within 'await' expression}}
    }
  }
}
