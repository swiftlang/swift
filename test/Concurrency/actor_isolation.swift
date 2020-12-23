// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

let immutableGlobal: String = "hello"
var mutableGlobal: String = "can't touch this" // expected-note 3{{var declared here}}

func globalFunc() { }
func acceptClosure<T>(_: () -> T) { }
func acceptEscapingClosure<T>(_: @escaping () -> T) { }
func acceptEscapingClosure<T>(_: (String) -> ()) async -> T? { nil }

func acceptAsyncClosure<T>(_: () async -> T) { }
func acceptEscapingAsyncClosure<T>(_: @escaping () async -> T) { }


// ----------------------------------------------------------------------
// Actor state isolation restrictions
// ----------------------------------------------------------------------
actor class MySuperActor {
  var superState: Int = 25 // expected-note {{mutable state is only available within the actor instance}}

  func superMethod() { } // expected-note 3 {{calls to instance method 'superMethod()' from outside of its actor context are implicitly asynchronous}}
  func superAsyncMethod() async { }

  subscript (index: Int) -> String { // expected-note 3{{subscript declared here}}
    "\(index)"
  }
}

actor class MyActor: MySuperActor {
  let immutable: Int = 17
  var text: [String] = [] // expected-note 9{{mutable state is only available within the actor instance}}

  class func synchronousClass() { }
  static func synchronousStatic() { }

  func synchronous() -> String { text.first ?? "nothing" } // expected-note 20{{calls to instance method 'synchronous()' from outside of its actor context are implicitly asynchronous}}
  func asynchronous() async -> String { synchronous() }
}

extension MyActor {
  @actorIndependent var actorIndependentVar: Int {
    get { 5 }
    set { }
  }

  // expected-note@+1 {{add 'async' to function 'actorIndependentFunc(otherActor:)' to make it asynchronous}} {{none}}
  @actorIndependent func actorIndependentFunc(otherActor: MyActor) -> Int {
    _ = immutable
    _ = text[0] // expected-error{{actor-isolated property 'text' can not be referenced from an '@actorIndependent' context}}
    _ = synchronous() // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from an '@actorIndependent' context}}

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

    // Global actors
    syncGlobalActorFunc() /// expected-error{{'async' in a function that does not support concurrency}}
    _ = syncGlobalActorFunc // expected-error{{global function 'syncGlobalActorFunc()' isolated to global actor 'SomeGlobalActor' can not be referenced from an '@actorIndependent' context}}

    // Global data is okay if it is immutable.
    _ = immutableGlobal
    _ = mutableGlobal // expected-warning{{reference to var 'mutableGlobal' is not concurrency-safe because it involves shared mutable state}}

    // Partial application
    _ = synchronous  // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from an '@actorIndependent' context}}
    _ = super.superMethod // expected-error{{actor-isolated instance method 'superMethod()' can not be referenced from an '@actorIndependent' context}}
    acceptClosure(synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from an '@actorIndependent' context}}
    acceptClosure(self.synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from an '@actorIndependent' context}}
    acceptClosure(otherActor.synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can only be referenced on 'self'}}
    acceptEscapingClosure(synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from an '@actorIndependent' context}}}}
    acceptEscapingClosure(self.synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from an '@actorIndependent'}}
    acceptEscapingClosure(otherActor.synchronous) // expected-error{{actor-isolated instance method 'synchronous()' can only be referenced on 'self'}}

    return 5
  }

  func testAsynchronous(otherActor: MyActor) async {
    _ = immutable
    _ = synchronous()
    _ = text[0]

    // Accesses on 'self' are okay.
    _ = self.immutable
    _ = self.synchronous()
    _ = await self.asynchronous()
    _ = self.text[0]
    _ = self[0]

    // Accesses on 'super' are okay.
    _ = super.superState
    super.superMethod()
    await super.superAsyncMethod()
    _ = super[0]

    // Accesses on other actors can only reference immutable data or
    // call methods
    _ = otherActor.immutable // okay
    _ = otherActor.synchronous() // expected-error{{call is 'async' but is not marked with 'await'}}
    _ = await otherActor.asynchronous()
    _ = otherActor.text[0] // expected-error{{actor-isolated property 'text' can only be referenced on 'self'}}

    // Global data is okay if it is immutable.
    _ = immutableGlobal
    _ = mutableGlobal // expected-warning{{reference to var 'mutableGlobal' is not concurrency-safe because it involves shared mutable state}}

    // Global functions are not actually safe, but we allow them for now.
    globalFunc()

    // Class methods are okay.
    Self.synchronousClass()
    Self.synchronousStatic()

    // Global actors
    syncGlobalActorFunc() // expected-error{{call is 'async' but is not marked with 'await'}}
    await asyncGlobalActorFunc()

    // Closures.
    let localConstant = 17
    var localVar = 17 // expected-note 3{{var declared here}}

    // Non-escaping closures are okay.
    acceptClosure {
      _ = text[0]
      _ = self.synchronous()
      _ = localVar
      _ = localConstant
    }

    // Escaping closures might run concurrently.
    acceptEscapingClosure {
      _ = self.text[0] // expected-error{{actor-isolated property 'text' is unsafe to reference in code that may execute concurrently}}
      _ = self.synchronous() // expected-error{{actor-isolated instance method 'synchronous()' is unsafe to reference in code that may execute concurrently}}
      _ = localVar // expected-warning{{local var 'localVar' is unsafe to reference in code that may execute concurrently}}
      _ = localConstant
    }

    // Local functions might run concurrently.
    func localFn1() {
      _ = self.text[0] // expected-error{{actor-isolated property 'text' is unsafe to reference in code that may execute concurrently}}
      _ = self.synchronous() // expected-error{{actor-isolated instance method 'synchronous()' is unsafe to reference in code that may execute concurrently}}
      _ = localVar // expected-warning{{local var 'localVar' is unsafe to reference in code that may execute concurrently}}
      _ = localConstant
    }

    func localFn2() {
      acceptClosure {
        _ = text[0]  // expected-error{{actor-isolated property 'text' is unsafe to reference in code that may execute concurrently}}
        _ = self.synchronous() // expected-error{{actor-isolated instance method 'synchronous()' is unsafe to reference in code that may execute concurrently}}
        _ = localVar // expected-warning{{local var 'localVar' is unsafe to reference in code that may execute concurrently}}
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
    _ = super.superMethod // expected-error{{actor-isolated instance method 'superMethod()' is unsafe to reference in code that may execute concurrently}}
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
actor class SomeActor { }

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

@globalActor
struct SomeOtherGlobalActor {
  static let shared = SomeActor()
}

@globalActor
struct GenericGlobalActor<T> {
  static var shared: SomeActor { SomeActor() }
}

@SomeGlobalActor func syncGlobalActorFunc() { syncGlobalActorFunc() } // expected-note{{calls to global function 'syncGlobalActorFunc()' from outside of its actor context are implicitly asynchronous}}
@SomeGlobalActor func asyncGlobalActorFunc() async { await asyncGlobalActorFunc() }

@SomeOtherGlobalActor func syncOtherGlobalActorFunc() { }

@SomeOtherGlobalActor func asyncOtherGlobalActorFunc() async {
  await syncGlobalActorFunc()
  await asyncGlobalActorFunc()
}

// test global actor funcs that are marked asyncHandler
@SomeGlobalActor func goo1() async {
  let _ = goo2
  goo2()
}
@asyncHandler @SomeOtherGlobalActor func goo2() { await goo1() }

extension MyActor {
  @SomeGlobalActor func onGlobalActor(otherActor: MyActor) async {
    // Access to other functions in this actor are okay.
    syncGlobalActorFunc()
    await asyncGlobalActorFunc()

    // Other global actors are ok if marked with 'await'
    await syncOtherGlobalActorFunc()
    await asyncOtherGlobalActorFunc()

    _ = immutable
    _ = synchronous() // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from context of global actor 'SomeGlobalActor'}}
    _ = text[0] // expected-error{{actor-isolated property 'text' can not be referenced from context of global actor 'SomeGlobalActor'}}

    // Accesses on 'self' are only okay for immutable and asynchronous, because
    // we are outside of the actor instance.
    _ = self.immutable
    _ = self.synchronous() // expected-error{{actor-isolated instance method 'synchronous()' can not be referenced from context of global actor 'SomeGlobalActor'}}
    _ = await self.asynchronous()
    _ = self.text[0] // expected-error{{actor-isolated property 'text' can not be referenced from context of global actor 'SomeGlobalActor'}}
    _ = self[0] // expected-error{{actor-isolated subscript 'subscript(_:)' can not be referenced from context of global actor 'SomeGlobalActor'}}

    // Accesses on 'super' are not okay; we're outside of the actor.
    _ = super.superState // expected-error{{actor-isolated property 'superState' can not be referenced from context of global actor 'SomeGlobalActor'}}
    super.superMethod() // expected-error{{actor-isolated instance method 'superMethod()' can not be referenced from context of global actor 'SomeGlobalActor'}}
    await super.superAsyncMethod()
    _ = super[0] // expected-error{{actor-isolated subscript 'subscript(_:)' can not be referenced from context of global actor 'SomeGlobalActor'}}

    // Accesses on other actors can only reference immutable data or
    // call asychronous methods
    _ = otherActor.immutable // okay
    _ = otherActor.synchronous() // expected-error{{call is 'async' but is not marked with 'await'}}
    _ = otherActor.synchronous  // expected-error{{actor-isolated instance method 'synchronous()' can only be referenced on 'self'}}
    _ = await otherActor.asynchronous()
    _ = otherActor.text[0] // expected-error{{actor-isolated property 'text' can only be referenced on 'self'}}
  }
}

struct GenericStruct<T> {
  @GenericGlobalActor<T> func f() { } // expected-note{{calls to instance method 'f()' from outside of its actor context are implicitly asynchronous}}

  @GenericGlobalActor<T> func g() {
    f() // okay
  }

  // expected-note@+2 {{add '@asyncHandler' to function 'h()' to create an implicit asynchronous context}} {{3-3=@asyncHandler }}
  // expected-note@+1 {{add 'async' to function 'h()' to make it asynchronous}} {{none}}
  @GenericGlobalActor<String> func h() {
    f() // expected-error{{'async' in a function that does not support concurrency}}
    _ = f // expected-error{{instance method 'f()' isolated to global actor 'GenericGlobalActor<T>' can not be referenced from different global actor 'GenericGlobalActor<String>'}}
  }
}

extension GenericStruct where T == String {
  @GenericGlobalActor<T>
  func h2() {
    f()
    g()
    h()
  }
}


// ----------------------------------------------------------------------
// Non-actor code isolation restrictions
// ----------------------------------------------------------------------
func testGlobalRestrictions(actor: MyActor) async {
  let _ = MyActor()

  // Asynchronous functions and immutable state are permitted.
  _ = await actor.asynchronous()
  _ = actor.immutable

  // Synchronous operations are ok, mutable state references are not.
  _ = actor.synchronous // expected-error{{actor-isolated instance method 'synchronous()' can only be referenced inside the actor}}
  _ = actor.synchronous() // expected-error{{call is 'async' but is not marked with 'await'}}
  _ = actor.text[0] // expected-error{{actor-isolated property 'text' can only be referenced inside the actor}}
  _ = actor[0] // expected-error{{actor-isolated subscript 'subscript(_:)' can only be referenced inside the actor}}

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
  var i = 17 // expected-note{{var declared here}}
  acceptEscapingClosure {
    i = 42 // expected-warning{{local var 'i' is unsafe to reference in code that may execute concurrently}}
  }
  print(i)
}

// ----------------------------------------------------------------------
// Local function isolation restrictions
// ----------------------------------------------------------------------
func checkLocalFunctions() async {
  var i = 0
  var j = 0 // expected-note{{var declared here}}

  func local1() {
    i = 17
  }

  func local2() {
    j = 42 // expected-warning{{local var 'j' is unsafe to reference in code that may execute concurrently}}
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
  acceptEscapingClosure {
    local2()
  }

  print(i)
  print(j)

  var k = 17 // expected-note{{var declared here}}
  func local4() {
    acceptEscapingClosure {
      local3()
    }
  }

  func local3() {
    k = 25 // expected-warning{{local var 'k' is unsafe to reference in code that may execute concurrently}}
  }

  print(k)
}
