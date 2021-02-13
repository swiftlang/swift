// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

let immutableGlobal: String = "hello"
var mutableGlobal: String = "can't touch this" // expected-note 5{{var declared here}}

func globalFunc() { }
func acceptClosure<T>(_: () -> T) { }
func acceptConcurrentClosure<T>(_: @concurrent () -> T) { }
func acceptEscapingClosure<T>(_: @escaping () -> T) { }
func acceptEscapingClosure<T>(_: @escaping (String) -> ()) async -> T? { nil }

func acceptAsyncClosure<T>(_: () async -> T) { }
func acceptEscapingAsyncClosure<T>(_: @escaping () async -> T) { }


// ----------------------------------------------------------------------
// Actor state isolation restrictions
// ----------------------------------------------------------------------
actor MySuperActor {
  var superState: Int = 25 // expected-note {{mutable state is only available within the actor instance}}

  func superMethod() { } // expected-note 3 {{calls to instance method 'superMethod()' from outside of its actor context are implicitly asynchronous}}
  func superAsyncMethod() async { }

  subscript (index: Int) -> String { // expected-note 3{{subscript declared here}}
    "\(index)"
  }
}

actor MyActor: MySuperActor {
  let immutable: Int = 17
  var text: [String] = [] // expected-note 10{{mutable state is only available within the actor instance}}

  class func synchronousClass() { }
  static func synchronousStatic() { }

  func synchronous() -> String { text.first ?? "nothing" } // expected-note 21{{calls to instance method 'synchronous()' from outside of its actor context are implicitly asynchronous}}
  func asynchronous() async -> String { synchronous() }
}

extension MyActor {
  @actorIndependent var actorIndependentVar: Int {
    get { 5 }
    set { }
  }

  // expected-note@+1 {{add 'async' to function 'actorIndependentFunc(otherActor:)' to make it asynchronous}} {{67-67= async}}
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
    var localVar = 17

    // Non-escaping closures are okay.
    acceptClosure {
      _ = text[0]
      _ = self.synchronous()
      _ = localVar
      _ = localConstant
    }

    // Concurrent closures might run... concurrently.
    acceptConcurrentClosure {
      _ = self.text[0] // expected-error{{actor-isolated property 'text' cannot be referenced from a concurrent closure}}
      _ = self.synchronous() // expected-error{{actor-isolated instance method 'synchronous()' cannot be referenced from a concurrent closure}}
      _ = localVar // expected-error{{reference to captured var 'localVar' in concurrently-executing code}}
      localVar = 25 // expected-error{{mutation of captured var 'localVar' in concurrently-executing code}}
      _ = localConstant
    }

    // Escaping closures might run concurrently.
    acceptEscapingClosure {
      _ = self.text[0] // expected-error{{actor-isolated property 'text' cannot be referenced from an '@escaping' closure}}
      _ = self.synchronous() // expected-error{{actor-isolated instance method 'synchronous()' cannot be referenced from an '@escaping' closure}}
      _ = localVar // okay, don't complain about escaping
      _ = localConstant
    }

    // Local functions might run concurrently.
    @concurrent func localFn1() {
      _ = self.text[0] // expected-error{{actor-isolated property 'text' cannot be referenced from a concurrent function}}
      _ = self.synchronous() // expected-error{{actor-isolated instance method 'synchronous()' cannot be referenced from a concurrent function}}
      _ = localVar // expected-error{{reference to captured var 'localVar' in concurrently-executing code}}
      localVar = 25 // expected-error{{mutation of captured var 'localVar' in concurrently-executing code}}
      _ = localConstant
    }

    @concurrent func localFn2() {
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
    _ = super.superMethod // expected-error{{actor-isolated instance method 'superMethod()' can not be referenced from an '@actorIndependent' context}}
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
actor SomeActor { }

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
  // expected-note@+1 {{add 'async' to function 'h()' to make it asynchronous}} {{39-39= async}}
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
  var i = 17
  acceptConcurrentClosure {
    _ = i // expected-error{{reference to captured var 'i' in concurrently-executing code}}
    i = 42 // expected-error{{mutation of captured var 'i' in concurrently-executing code}}
  }
  print(i)

  acceptConcurrentClosure { [i] in
    _ = i
  }
}

func f() {
  acceptConcurrentClosure {
    _ = mutableGlobal // expected-warning{{reference to var 'mutableGlobal' is not concurrency-safe because it involves shared mutable state}}
  }

  @concurrent func g() {
    _ = mutableGlobal // expected-warning{{reference to var 'mutableGlobal' is not concurrency-safe because it involves shared mutable state}}
  }
}

// ----------------------------------------------------------------------
// Local function isolation restrictions
// ----------------------------------------------------------------------
func checkLocalFunctions() async {
  var i = 0
  var j = 0

  func local1() {
    i = 17
  }

  func local2() { // expected-error{{concurrently-executed local function 'local2()' must be marked as '@concurrent'}}{{3-3=@concurrent }}
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

  func local3() { // expected-error{{concurrently-executed local function 'local3()' must be marked as '@concurrent'}}
    k = 25 // expected-error{{mutation of captured var 'k' in concurrently-executing code}}
  }

  print(k)
}

// ----------------------------------------------------------------------
// Lazy properties with initializers referencing 'self'
// ----------------------------------------------------------------------

actor LazyActor {
    var v: Int = 0
    // expected-note@-1 6 {{mutable state is only available within the actor instance}}

    let l: Int = 0

    lazy var l11: Int = { v }()
    lazy var l12: Int = v
    lazy var l13: Int = { self.v }()
    lazy var l14: Int = self.v
    lazy var l15: Int = { [unowned self] in self.v }() // expected-error{{actor-isolated property 'v' can not be referenced from an '@actorIndependent' context}}

    lazy var l21: Int = { l }()
    lazy var l22: Int = l
    lazy var l23: Int = { self.l }()
    lazy var l24: Int = self.l
    lazy var l25: Int = { [unowned self] in self.l }()

    @actorIndependent lazy var l31: Int = { v }()
    // expected-error@-1 {{actor-isolated property 'v' can not be referenced from an '@actorIndependent' context}}
    @actorIndependent lazy var l32: Int = v
    // expected-error@-1 {{actor-isolated property 'v' can not be referenced from an '@actorIndependent' context}}
    @actorIndependent lazy var l33: Int = { self.v }()
    // expected-error@-1 {{actor-isolated property 'v' can not be referenced from an '@actorIndependent' context}}
    @actorIndependent lazy var l34: Int = self.v
    // expected-error@-1 {{actor-isolated property 'v' can not be referenced from an '@actorIndependent' context}}
    @actorIndependent lazy var l35: Int = { [unowned self] in self.v }()
    // expected-error@-1 {{actor-isolated property 'v' can not be referenced from an '@actorIndependent' context}}

    @actorIndependent lazy var l41: Int = { l }()
    @actorIndependent lazy var l42: Int = l
    @actorIndependent lazy var l43: Int = { self.l }()
    @actorIndependent lazy var l44: Int = self.l
    @actorIndependent lazy var l45: Int = { [unowned self] in self.l }()
}

// Infer global actors from context only for instance members.
@MainActor
class SomeClassInActor {
  enum ID: String { case best }

  func inActor() { } // expected-note{{calls to instance method 'inActor()' from outside of its actor context are implicitly asynchronous}}
}

extension SomeClassInActor.ID {
  func f(_ object: SomeClassInActor) { // expected-note{{add '@MainActor' to make instance method 'f' part of global actor 'MainActor'}}
    // expected-note@-1{{add 'async' to function 'f' to make it asynchronous}}
    // expected-note@-2{{add '@asyncHandler' to function 'f' to create an implicit asynchronous context}}
    object.inActor() // expected-error{{'async' in a function that does not support concurrency}}
  }
}

// ----------------------------------------------------------------------
// Initializers
// ----------------------------------------------------------------------
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

@MainActor
class SomeClassWithInits {
  var mutableState: Int = 17
  var otherMutableState: Int

  init() {
    self.mutableState = 42
    self.otherMutableState = 17

    self.isolated()
  }

  func isolated() { }
}
