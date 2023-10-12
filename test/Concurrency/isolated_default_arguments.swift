// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/Inputs/OtherActors.swift -disable-availability-checking

// RUN: %target-swift-frontend -I %t  -disable-availability-checking -strict-concurrency=complete -enable-experimental-feature IsolatedDefaultValues -parse-as-library -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -I %t  -disable-availability-checking -strict-concurrency=complete -parse-as-library -emit-sil -o /dev/null -verify -enable-experimental-feature IsolatedDefaultValues -enable-experimental-feature SendNonSendable %s

// REQUIRES: concurrency
// REQUIRES: asserts

@globalActor
actor SomeGlobalActor {
  static let shared = SomeGlobalActor()
}

// expected-note@+2 2 {{calls to global function 'requiresMainActor()' from outside of its actor context are implicitly asynchronous}}
@MainActor
func requiresMainActor() -> Int { 0 }

@SomeGlobalActor
func requiresSomeGlobalActor() -> Int { 0 }

func mainActorDefaultArg(value: Int = requiresMainActor()) {}

func mainActorClosure(
  closure: () -> Int = {
    requiresMainActor()
  }
) {}

func mainActorClosureCall(
  closure: Int = {
    requiresMainActor()
  }()
) {}

@MainActor func mainActorCaller() {
  mainActorDefaultArg()
  mainActorClosure()
  mainActorClosureCall()
}

func nonisolatedCaller() {
  // expected-error@+1 {{main actor-isolated default argument cannot be synchronously evaluated from a nonisolated context}}
  mainActorDefaultArg()

  // FIXME: confusing error message.
  // expected-error@+1 {{main actor-isolated default argument cannot be synchronously evaluated from a nonisolated context}}
  mainActorClosure()

  // expected-error@+1 {{main actor-isolated default argument cannot be synchronously evaluated from a nonisolated context}}
  mainActorClosureCall()
}

func nonisolatedAsyncCaller() async {
  // expected-error@+1 {{main actor-isolated default argument cannot be synchronously evaluated from a nonisolated context}}
  mainActorDefaultArg()

  // expected-error@+1 {{main actor-isolated default argument cannot be synchronously evaluated from a nonisolated context}}
  mainActorClosure()

  // expected-error@+1 {{main actor-isolated default argument cannot be synchronously evaluated from a nonisolated context}}
  mainActorClosureCall()

  await mainActorDefaultArg(value: requiresMainActor())

  // 'await' doesn't help closures in default arguments; the calling context needs a matching
  // actor isolation for that isolation to be inferred for the closure value.

  // expected-error@+2 {{main actor-isolated default argument cannot be synchronously evaluated from a nonisolated context}}
  // expected-warning@+1 {{no 'async' operations occur within 'await' expression}}
  await mainActorClosure()

  // expected-error@+2 {{main actor-isolated default argument cannot be synchronously evaluated from a nonisolated context}}
  // expected-warning@+1 {{no 'async' operations occur within 'await' expression}}
  await mainActorClosureCall()
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

@MainActor
struct S1 {
  var required: Int

  var x: Int = requiresMainActor()

  lazy var y: Int = requiresMainActor()

  static var z: Int = requiresMainActor()
}

@SomeGlobalActor
struct S2 {
  var required: Int

  var x: Int = requiresSomeGlobalActor()

  lazy var y: Int = requiresSomeGlobalActor()

  static var z: Int = requiresSomeGlobalActor()
}

struct S3 {
  // expected-error@+1 {{default argument cannot be both main actor-isolated and global actor 'SomeGlobalActor'-isolated}}
  var (x, y, z) = (requiresMainActor(), requiresSomeGlobalActor(), 10)
}

@MainActor
func initializeFromMainActor() {
  _ = S1(required: 10)

  // expected-error@+1 {{global actor 'SomeGlobalActor'-isolated default argument cannot be synchronously evaluated from a main actor-isolated context}}
  _ = S2(required: 10)
}

@SomeGlobalActor
func initializeFromSomeGlobalActor() {
  // expected-error@+1 {{main actor-isolated default argument cannot be synchronously evaluated from a global actor 'SomeGlobalActor'-isolated context}}
  _ = S1(required: 10)

  _ = S2(required: 10)
}

func initializeFromNonisolated() {
  // expected-error@+1 {{main actor-isolated default argument cannot be synchronously evaluated from a nonisolated context}}
  _ = S1(required: 10)

  // expected-error@+1 {{global actor 'SomeGlobalActor'-isolated default argument cannot be synchronously evaluated from a nonisolated context}}
  _ = S2(required: 10)
}

extension A {
  func initializeFromActorInstance() {
    // expected-error@+1 {{main actor-isolated default argument cannot be synchronously evaluated from a actor-isolated context}}
    _ = S1(required: 10)

    // expected-error@+1 {{global actor 'SomeGlobalActor'-isolated default argument cannot be synchronously evaluated from a actor-isolated context}}
    _ = S2(required: 10)
  }
}
