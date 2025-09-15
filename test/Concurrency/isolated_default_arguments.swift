// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -I %t  -target %target-swift-5.1-abi-triple -strict-concurrency=complete -parse-as-library -emit-sil -o /dev/null -verify -enable-upcoming-feature InferSendableFromCaptures %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_InferSendableFromCaptures

@globalActor
actor SomeGlobalActor {
  static let shared = SomeGlobalActor()
}

// expected-note@+2 2 {{calls to global function 'requiresMainActor()' from outside of its actor context are implicitly asynchronous}}
@MainActor
func requiresMainActor() -> Int { 0 }

@SomeGlobalActor
func requiresSomeGlobalActor() -> Int { 0 }

// expected-error@+1 {{main actor-isolated default value in a nonisolated context}}
func mainActorDefaultArgInvalid(value: Int = requiresMainActor()) {}

func mainActorClosureInvalid(
  closure: () -> Int = { // expected-error {{main actor-isolated default value in a nonisolated context}}
    requiresMainActor()
  }
) {}

// expected-note@+2 {{calls to global function 'mainActorDefaultArg(value:)' from outside of its actor context are implicitly asynchronous}}
@MainActor
func mainActorDefaultArg(value: Int = requiresMainActor()) {}

// expected-note@+1 {{calls to global function 'mainActorClosure(closure:)' from outside of its actor context are implicitly asynchronous}}
@MainActor func mainActorClosure(
  closure: () -> Int = {
    requiresMainActor()
  }
) {}

func mainActorClosureCall(
  closure: Int = { // expected-error {{main actor-isolated default value in a nonisolated context}}
    requiresMainActor()
  }()
) {}

@MainActor func mainActorCaller() {
  mainActorDefaultArg()
  mainActorClosure()
  mainActorClosureCall()
}

// expected-note@+1 2 {{add '@MainActor' to make global function 'nonisolatedCaller()' part of global actor 'MainActor'}}
func nonisolatedCaller() {
  // expected-error@+1 {{call to main actor-isolated global function 'mainActorDefaultArg(value:)' in a synchronous nonisolated context}}
  mainActorDefaultArg()

  // expected-error@+1 {{call to main actor-isolated global function 'mainActorClosure(closure:)' in a synchronous nonisolated context}}
  mainActorClosure()
}

func nonisolatedAsyncCaller() async {
  // expected-error@+1 {{main actor-isolated global function 'mainActorDefaultArg(value:)' cannot be called from outside of the actor}} {{3-3=await }}
  mainActorDefaultArg()

  // expected-error@+1 {{main actor-isolated global function 'mainActorClosure(closure:)' cannot be called from outside of the actor}} {{3-3=await }}
  mainActorClosure()

  await mainActorDefaultArg(value: requiresMainActor())

  await mainActorClosure()

  mainActorClosureCall()
}

func conflictingIsolationDefaultArg(
  // expected-error@+1 {{default argument cannot be both main actor-isolated and global actor 'SomeGlobalActor'-isolated}}
  value: (Int, Int) = (requiresMainActor(), requiresSomeGlobalActor())
) {}

func closureLosesIsolation(
  closure: @Sendable () -> Void = {
    // expected-note@+1 {{calls to local function 'f()' from outside of its actor context are implicitly asynchronous}}
    @MainActor func f() {}
    // expected-error@+1 {{call to main actor-isolated local function 'f()' in a synchronous nonisolated context}}
    f()
  }
) {}

func closureIsolationMismatch(
  closure: @SomeGlobalActor () -> Void = {
    // expected-note@+1 {{calls to local function 'f()' from outside of its actor context are implicitly asynchronous}}
    @MainActor func f() {}
    // expected-error@+1 {{call to main actor-isolated local function 'f()' in a synchronous global actor 'SomeGlobalActor'-isolated context}}
    f()
  }
) {}

func conflictingClosureIsolation(
  // expected-error@+1 {{default argument cannot be both main actor-isolated and global actor 'SomeGlobalActor'-isolated}}
  closure: () -> Void = {
    _ = requiresMainActor()
    _ = requiresSomeGlobalActor()
  }
) {}

func isolationInLocalFunction(
  closure: () -> Void = {
    nonisolated func local() -> Int {
      // expected-error@+1 {{call to main actor-isolated global function 'requiresMainActor()' in a synchronous nonisolated context}}
      requiresMainActor()
    }
  }
) {}

actor A {}

func closureWithIsolatedParam(
  closure: (isolated A) -> Void = { _ in
    // expected-error@+1 {{call to main actor-isolated global function 'requiresMainActor()' in a synchronous actor-isolated context}}
    _ = requiresMainActor()
  }
) {}

// expected-note@+2 3 {{calls to initializer 'init(required:x:y:)' from outside of its actor context are implicitly asynchronous}}
@MainActor
struct S1 {
  var required: Int

  var x: Int = requiresMainActor()

  lazy var y: Int = requiresMainActor()

  static var z: Int = requiresMainActor()
}

// expected-note@+2 3 {{calls to initializer 'init(required:x:y:)' from outside of its actor context are implicitly asynchronous}}
@SomeGlobalActor
struct S2 {
  var required: Int

  var x: Int = requiresSomeGlobalActor()

  lazy var y: Int = requiresSomeGlobalActor()

  static var z: Int = requiresSomeGlobalActor()
}

struct S3 {
  // expected-error@+1 3 {{default argument cannot be both main actor-isolated and global actor 'SomeGlobalActor'-isolated}}
  var (x, y, z) = (requiresMainActor(), requiresSomeGlobalActor(), 10)
}

struct S4 {
  // expected-warning@+1 {{main actor-isolated default value in a nonisolated context; this is an error in the Swift 6 language mode}}
  var x: Int = requiresMainActor()
}

@MainActor
func initializeFromMainActor() {
  _ = S1(required: 10)

  // expected-error@+1 {{call to global actor 'SomeGlobalActor'-isolated initializer 'init(required:x:y:)' in a synchronous main actor-isolated context}}
  _ = S2(required: 10)
}

@SomeGlobalActor
func initializeFromSomeGlobalActor() {
  // expected-error@+1 {{call to main actor-isolated initializer 'init(required:x:y:)' in a synchronous global actor 'SomeGlobalActor'-isolated context}}
  _ = S1(required: 10)

  _ = S2(required: 10)
}

// expected-note@+2 {{add '@MainActor' to make global function 'initializeFromNonisolated()' part of global actor 'MainActor'}}
// expected-note@+1 {{add '@SomeGlobalActor' to make global function 'initializeFromNonisolated()' part of global actor 'SomeGlobalActor'}}
func initializeFromNonisolated() {
  // expected-error@+1 {{call to main actor-isolated initializer 'init(required:x:y:)' in a synchronous nonisolated context}}
  _ = S1(required: 10)

  // expected-error@+1 {{call to global actor 'SomeGlobalActor'-isolated initializer 'init(required:x:y:)' in a synchronous nonisolated context}}
  _ = S2(required: 10)
}

extension A {
  func initializeFromActorInstance() {
    // expected-error@+1 {{call to main actor-isolated initializer 'init(required:x:y:)' in a synchronous actor-isolated context}}
    _ = S1(required: 10)

    // expected-error@+1 {{call to global actor 'SomeGlobalActor'-isolated initializer 'init(required:x:y:)' in a synchronous actor-isolated context}}
    _ = S2(required: 10)
  }
}

// expected-warning@+1 {{default initializer for 'C1' cannot be both nonisolated and main actor-isolated; this is an error in the Swift 6 language mode}}
class C1 {
  // expected-note@+1 {{initializer for property 'x' is main actor-isolated}}
  @MainActor var x = requiresMainActor()
  @SomeGlobalActor var y = requiresSomeGlobalActor()
}

class NonSendable {}

// expected-warning@+1 {{default initializer for 'C2' cannot be both nonisolated and main actor-isolated; this is an error in the Swift 6 language mode}}
class C2 {
  // expected-note@+1 {{initializer for property 'x' is main actor-isolated}}
  @MainActor var x = NonSendable()
  @SomeGlobalActor var y = NonSendable()
}

class C3 {
  @MainActor var x = 1
  @SomeGlobalActor var y = 2
}

class C4 {
  let task1 = Task {
    // expected-error@+1 {{main actor-isolated global function 'requiresMainActor()' cannot be called from outside of the actor}} {{5-5=await }}
    requiresMainActor()
  }

  let task2 = Task {
    await requiresMainActor() // okay
  }
}

@MainActor struct NonIsolatedInit {
  var x = 0
  var y = 0
}

@MainActor class MultipleVars {
  var (x, y) = (0, 0)
}

func callDefaultInit() async {
  _ = C2()
  _ = NonIsolatedInit()
  _ = NonIsolatedInit(x: 10)
  _ = MultipleVars()
}

// expected-warning@+1 {{default initializer for 'MultipleVarsInvalid' cannot be both nonisolated and main actor-isolated; this is an error in the Swift 6 language mode}}
class MultipleVarsInvalid {
  // expected-note@+1 {{initializer for property 'x' is main actor-isolated}}
  @MainActor var (x, y) = (requiresMainActor(), requiresMainActor())
}

@propertyWrapper 
@preconcurrency @MainActor
struct RequiresMain<Value>  {
  var wrappedValue: Value

  init(wrappedValue: Value) {
    self.wrappedValue = wrappedValue
  }
}

// This is okay; UseRequiresMain has an inferred 'MainActor'
// attribute.
struct UseRequiresMain {
  @RequiresMain private var x = 10
}

nonisolated func test() async {
  // expected-warning@+1 {{main actor-isolated initializer 'init()' cannot be called from outside of the actor; this is an error in the Swift 6 language mode}} {{7-7=await }}
  _ = UseRequiresMain()
}

// expected-warning@+2 {{memberwise initializer for 'InitAccessors' cannot be both nonisolated and main actor-isolated; this is an error in the Swift 6 language mode}}
// expected-warning@+1 {{default initializer for 'InitAccessors' cannot be both nonisolated and main actor-isolated; this is an error in the Swift 6 language mode}}
struct InitAccessors {
  private var _a: Int

  // expected-note@+1 2 {{initializer for property 'a' is main actor-isolated}}
  @MainActor var a: Int = 5 {
    @storageRestrictions(initializes: _a)
    init {
      _a = requiresMainActor()
    }
    get {
      _a
    }
  }
}

// Make sure isolation inference for implicit initializers
// doesn't impact conformance synthesis.

struct CError: Error, RawRepresentable {
  var rawValue: CInt
}

// Consider isolated key-paths when computing initializer isolation

@MainActor
class UseIsolatedKeyPath {
  let kp: KeyPath<UseIsolatedKeyPath, Nested> = \.x // okay

  // expected-error@+1 {{default argument cannot be both main actor-isolated and global actor 'SomeGlobalActor'-isolated}}
  let kp2: KeyPath<UseIsolatedKeyPath, Bool> = \.x.y // okay

  var x: Nested = .init()

  class Nested {
    @SomeGlobalActor var y: Bool = true
  }
}

@MainActor
protocol InferMainActor {}

struct UseIsolatedPropertyWrapperInit: InferMainActor {
  @Wrapper(\.value) var value: Int // okay

  // expected-warning@+1 {{global actor 'SomeGlobalActor'-isolated default value in a main actor-isolated context; this is an error in the Swift 6 language mode}}
  @Wrapper(\.otherValue) var otherValue: Int
}

@propertyWrapper struct Wrapper<T> {
  init(_: KeyPath<Values, T>) {}
  var wrappedValue: T { fatalError() }
}

struct Values {
  @MainActor var value: Int { 0 }
  @SomeGlobalActor var otherValue: Int { 0 }
}

class PreconcurrencyInit {
  @preconcurrency @MainActor init() {}
}

// expected-warning@+1 {{main actor-isolated default value in a nonisolated context; this is an error in the Swift 6 language mode}}
func downgrade(_: PreconcurrencyInit = .init()) {}
