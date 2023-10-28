// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/Inputs/OtherActors.swift -disable-availability-checking

// RUN: %target-swift-frontend -I %t  -disable-availability-checking -strict-concurrency=complete -enable-experimental-feature IsolatedDefaultValues -parse-as-library -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -I %t  -disable-availability-checking -strict-concurrency=complete -parse-as-library -emit-sil -o /dev/null -verify -enable-experimental-feature IsolatedDefaultValues -enable-experimental-feature RegionBasedIsolation %s

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

// expected-error@+1 {{main actor-isolated default argument in a nonisolated context}}
func mainActorDefaultArgInvalid(value: Int = requiresMainActor()) {}

func mainActorClosureInvalid(
  closure: () -> Int = { // expected-error {{main actor-isolated default argument in a nonisolated context}}
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
  closure: Int = { // expected-error {{main actor-isolated default argument in a nonisolated context}}
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
  // expected-error@+2 {{expression is 'async' but is not marked with 'await'}}
  // expected-note@+1 {{calls to global function 'mainActorDefaultArg(value:)' from outside of its actor context are implicitly asynchronous}}
  mainActorDefaultArg()

  // expected-error@+2 {{expression is 'async' but is not marked with 'await'}}
  // expected-note@+1 {{calls to global function 'mainActorClosure(closure:)' from outside of its actor context are implicitly asynchronous}}
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
  // expected-error@+1 {{default argument cannot be both main actor-isolated and global actor 'SomeGlobalActor'-isolated}}
  var (x, y, z) = (requiresMainActor(), requiresSomeGlobalActor(), 10)
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
