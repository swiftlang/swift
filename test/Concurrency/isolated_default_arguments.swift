// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/Inputs/OtherActors.swift -disable-availability-checking

// RUN: %target-swift-frontend -I %t  -disable-availability-checking -strict-concurrency=complete -enable-experimental-feature IsolatedDefaultArguments -parse-as-library -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -I %t  -disable-availability-checking -strict-concurrency=complete -parse-as-library -emit-sil -o /dev/null -verify -enable-experimental-feature IsolatedDefaultArguments -enable-experimental-feature SendNonSendable %s

// REQUIRES: concurrency
// REQUIRES: asserts

@globalActor
actor SomeGlobalActor {
  static let shared = SomeGlobalActor()
}

@MainActor
func requiresMainActor() -> Int { 0 }

// expected-note@+2 2 {{calls to global function 'requiresSomeGlobalActor()' from outside of its actor context are implicitly asynchronous}}
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
  // expected-error@+1 {{call to global actor 'SomeGlobalActor'-isolated global function 'requiresSomeGlobalActor()' in a synchronous nonisolated context}}
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
  closure: () -> Void = {
    _ = requiresMainActor()
    // expected-error@+1 {{call to global actor 'SomeGlobalActor'-isolated global function 'requiresSomeGlobalActor()' in a synchronous nonisolated context}}
    _ = requiresSomeGlobalActor()
  }
) {}
