// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -enable-experimental-feature AllowIsolatedGlobalActorAttribute -parse-as-library -verify-additional-prefix minimal- -verify-additional-prefix swift5-
// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -enable-experimental-feature AllowIsolatedGlobalActorAttribute -strict-concurrency=complete -parse-as-library -verify-additional-prefix complete- -verify-additional-prefix swift5-
// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -enable-experimental-feature AllowIsolatedGlobalActorAttribute -swift-version 6 -parse-as-library -verify-additional-prefix swift6-

// REQUIRES: concurrency
// REQUIRES: swift_feature_AllowIsolatedGlobalActorAttribute

// Concrete global actors are covered by
// isolated_global_actor_function_types_equivalence.swift, which re-runs the
// '@G' function type tests with the '@isolated(G)' spelling. This test covers
// the generic global actors that only the '@isolated(G)' spelling can name,
// following the structure of global_actor_function_types.swift
//
// Global actor isolated function types are implicitly '@Sendable' in the Swift
// 6 language mode, so the function types are explicitly '@Sendable' where that
// affects how they are printed

@globalActor
actor SomeGlobalActor {
  static let shared = SomeGlobalActor()
}

@SomeGlobalActor var counter = 0 // expected-note 2{{mutation of this var is only permitted within the actor}}

@MainActor var mainCounter = 0 // expected-note{{mutation of this var is only permitted within the actor}}

struct NotAGlobalActor {}

actor NotGlobalActor {}

protocol RefinedGlobalActor: GlobalActor {}

// ==== -----------------------------------------------------------------------
// MARK: Spelling equivalence for concrete global actors

func takesIsolated(_ fn: @isolated(SomeGlobalActor) () -> Void) {}

func takesCustom(_ fn: @SomeGlobalActor () -> Void) {
  takesIsolated(fn)
}

// Both spellings produce the same exact type and declaration:
func redeclared(_ fn: @MainActor () -> Void) {} // expected-note{{'redeclared' previously declared here}}
func redeclared(_ fn: @isolated(MainActor) () -> Void) {} // expected-error{{invalid redeclaration of 'redeclared'}}

func printsAsCustomAttribute(_ fn: @escaping @Sendable @isolated(SomeGlobalActor) (Int) -> Void) {
  let _: Int = fn // expected-error{{cannot convert value of type '@SomeGlobalActor @Sendable (Int) -> Void' to specified type 'Int'}}
}

// ==== -----------------------------------------------------------------------
// MARK: Conversions

func testConversions<G: GlobalActor, H: GlobalActor>(
  sendableIsoG: @escaping @Sendable @isolated(G) (Int) -> Void,
  sendable: @escaping @Sendable (Int) -> Void,
  isoH: @escaping @isolated(H) (Int) -> Void,
  isoMain: @escaping @MainActor (Int) -> Void
) {
  let _: Int = sendableIsoG // expected-error{{cannot convert value of type '@isolated(G) @Sendable (Int) -> Void' to specified type 'Int'}}

  let _: (Int) -> Void = sendableIsoG
  // expected-swift5-warning@-1{{converting function value of type '@isolated(G) @Sendable (Int) -> Void' to '(Int) -> Void' loses global actor 'G'}}
  // expected-swift6-error@-2{{converting function value of type '@isolated(G) @Sendable (Int) -> Void' to '(Int) -> Void' loses global actor 'G'}}
  let _: @isolated(G) (Int) -> Void = sendable // okay
  let _: @isolated(G) (Int) -> Void = sendableIsoG // okay

  let _: @isolated(H) (Int) -> Void = sendableIsoG // expected-error{{cannot convert value actor-isolated to 'G' to specified type actor-isolated to 'H'}}
  let _: @isolated(G) (Int) -> Void = isoH // expected-error{{cannot convert value actor-isolated to 'H' to specified type actor-isolated to 'G'}}

  // A generic global actor is never known to be the same as a concrete one
  let _: @MainActor (Int) -> Void = sendableIsoG // expected-error{{cannot convert value actor-isolated to 'G' to specified type actor-isolated to 'MainActor'}}
  let _: @isolated(G) (Int) -> Void = isoMain // expected-error{{cannot convert value actor-isolated to 'MainActor' to specified type actor-isolated to 'G'}}
  let _ = isoMain as @isolated(G) (Int) -> Void // expected-error{{cannot convert value actor-isolated to 'MainActor' to function actor-isolated to 'G' in coercion}}
}

struct Box<G: GlobalActor> {}

// With 'G == MainActor', '@isolated(G)' is the same as '@MainActor'
extension Box where G == MainActor {
  func sameTypeRequirement(
    _ f: @escaping @isolated(G) () -> Void,
    _ m: @escaping @MainActor () -> Void
  ) {
    let _: @MainActor () -> Void = f // okay
    let _: @isolated(G) () -> Void = m // okay
  }

  @MainActor
  func callsSynchronously(_ f: @isolated(G) () -> Void) {
    f() // okay
  }

  func closureIsolatedToMainActor() {
    let _: @isolated(G) () -> Void = {
      mainCounter += 1 // okay
    }
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Positions

struct X<T> {} // expected-note{{arguments to generic parameter 'T' ('@isolated(G) @Sendable () -> Void' and '() -> Void') are expected to be equal}}

func optional<G: GlobalActor>(_: (@isolated(G) () -> Void)?, _: G.Type) {}

// The extra Sendable diagnostics also happen for 'X<@Sendable @MainActor () -> Void>'
func genericArgument<G: GlobalActor>(_ x: X<@Sendable @isolated(G) () -> Void>) { // expected-note{{parameter 'x' is implicitly non-Sendable}}
  let _: X<() -> Void> = x
  // expected-error@-1{{cannot assign value of type 'X<@isolated(G) @Sendable () -> Void>' to type 'X<() -> Void>'}}
  // expected-error@-2{{using non-Sendable parameter 'x' in a context expecting a '@Sendable' closure}}
}

func collections<G: GlobalActor>(
  _ y: @escaping @isolated(G) () -> Void,
  _ m: @escaping @MainActor () -> Void
) {
  let _: [@isolated(G) () -> Void] = [y]
  let _: [@isolated(G) () -> Void] = [y, m] // expected-error{{cannot convert value actor-isolated to 'MainActor' to expected element type actor-isolated to 'G'}}
  let _: [Int: @isolated(G) () -> Void] = [1: y]
  let _: (@isolated(G) () -> Void, @MainActor () -> Void) = (y, m)
  let _: (@MainActor () -> Void, @isolated(G) () -> Void) = (y, y) // expected-error{{cannot convert type actor-isolated to 'G' to type actor-isolated to 'MainActor' at tuple element '#0'}}
}

func result<G: GlobalActor>(_: G.Type) -> @isolated(G) () -> Void {
  return {}
}

func curried<G: GlobalActor>(_: G.Type) {
  let _: () -> @isolated(G) () -> Void = {
    return {}
  }
}

struct Storage<G: GlobalActor> {
  var stored: @isolated(G) () -> Void
  let sendable: @Sendable @isolated(G) () -> Void
}

typealias IsolatedTo<G: GlobalActor> = @isolated(G) () -> Void

func usesTypealias<G: GlobalActor>(_ fn: @escaping IsolatedTo<G>) {
  let _: @isolated(G) () -> Void = fn
}

func autoclosure<G: GlobalActor>(_ x: @autoclosure @isolated(G) () -> Int, _: G.Type) {}

func refinedProtocol<G: RefinedGlobalActor>(_ fn: @isolated(G) () -> Void) {}

func whereClause<G>(_ fn: @isolated(G) () -> Void) where G: GlobalActor {}

extension GlobalActor {
  static func selfIsolated(_ fn: @isolated(Self) () -> Void) {}
}

// ==== -----------------------------------------------------------------------
// MARK: Isolation of calls and closures

func callsSync<G: GlobalActor>(_ fn: @isolated(G) () -> Void) { // expected-note{{calls to parameter 'fn' from outside of its actor context are implicitly asynchronous}}
  fn() // expected-error{{call to global actor 'G'-isolated parameter 'fn' in a synchronous nonisolated context}}
}

func callsAsync<G: GlobalActor>(_ fn: @isolated(G) () -> Void) async {
  await fn()
}

func closureInheritsGenericIsolation<G: GlobalActor>(
  _ inner: @escaping @isolated(G) () -> Int,
  _ main: @escaping @MainActor () -> Int // expected-note{{calls to parameter 'main' from outside of its actor context are implicitly asynchronous}}
) {
  // The closure is isolated to 'G', so it can call other 'G' functions
  // synchronously, but not functions isolated to a different global actor
  let _: @isolated(G) () -> Void = {
    _ = inner()
    _ = main() // expected-error{{call to main actor-isolated parameter 'main' in a synchronous global actor 'G'-isolated context}}
    counter += 1 // expected-error{{global actor 'SomeGlobalActor'-isolated var 'counter' can not be mutated from global actor 'G'}}
  }

  // Async closures can hop
  let _: @isolated(G) () async -> Void = {
    _ = inner()
    _ = await main()
    await MainActor.run { mainCounter += 1 }
  }
}

func sendableAndIsolated<G: GlobalActor>(
  _ f: @escaping @Sendable @isolated(G) () -> Void
) {
  let _: @Sendable () -> Void = f
  // expected-swift5-warning@-1{{converting function value of type '@isolated(G) @Sendable () -> Void' to '@Sendable () -> Void' loses global actor 'G'}}
  // expected-swift6-error@-2{{converting function value of type '@isolated(G) @Sendable () -> Void' to '@Sendable () -> Void' loses global actor 'G'}}
  let _: @isolated(G) () -> Void = f // okay
}

// ==== -----------------------------------------------------------------------
// MARK: Inferring the generic global actor from the argument

func inferredGeneric<G: GlobalActor>( // expected-note{{in call to function 'inferredGeneric'}}
  _ fn: @escaping @isolated(G) () -> Int
) async -> Int {
  await fn()
}

func infersGlobalActor(_ typed: @escaping @SomeGlobalActor () -> Int) async {
  // An argument that is already global actor isolated binds 'G'
  _ = await inferredGeneric(typed)

  // So does an explicit global actor on the closure
  _ = await inferredGeneric { @SomeGlobalActor in counter }

  // The closure body does not determine the closure's isolation
  _ = await inferredGeneric { 42 } // expected-error{{generic parameter 'G' could not be inferred}}
}

// ==== -----------------------------------------------------------------------
// MARK: assumeIsolated on any global actor

func assumesIsolated() {
  SomeGlobalActor.assumeIsolated {
    counter += 1
  }
  let value = SomeGlobalActor.assumeIsolated { counter }
  _ = value

  MainActor.assumeIsolated {
    mainCounter += 1
  }

  SomeGlobalActor.assumeIsolated {
    mainCounter += 1 // expected-error{{main actor-isolated var 'mainCounter' can not be mutated from global actor 'SomeGlobalActor'}}
  }
}

func assumesIsolatedGeneric<G: GlobalActor>(_: G.Type, _ fn: @isolated(G) () -> Void) {
  G.assumeIsolated {
    fn()
  }
  G.assumeIsolated {
    counter += 1 // expected-error{{global actor 'SomeGlobalActor'-isolated var 'counter' can not be mutated from global actor 'G'}}
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Invalid uses

func notGlobalActorGeneric<T>(_: T.Type, _ fn: @isolated(T) () -> Void) {} // expected-error{{type 'T' in '@isolated' attribute is not a global actor}}

func notGlobalActorConcrete(_ fn: @isolated(NotAGlobalActor) () -> Void) {} // expected-error{{type 'NotAGlobalActor' in '@isolated' attribute is not a global actor}}

// A type that only contains a generic global actor is not a global actor
func notGlobalActorOptional<G: GlobalActor>(_: G.Type, _ fn: @isolated(G?) () -> Void) {} // expected-error{{type 'G?' in '@isolated' attribute is not a global actor}}

func notGlobalActorString(_ fn: @isolated(String) () -> Void) {} // expected-error{{type 'String' in '@isolated' attribute is not a global actor}}

// An actor is not a global actor unless it is declared '@globalActor'
func notGlobalActorPlainActor(_ fn: @isolated(NotGlobalActor) () -> Void) {} // expected-error{{type 'NotGlobalActor' in '@isolated' attribute is not a global actor}}

func notGlobalActorActor(_ fn: @isolated(Actor) () -> Void) {} // expected-error{{type 'any Actor' in '@isolated' attribute is not a global actor}}

func unknownType(_ fn: @isolated(Nope) () -> Void) {} // expected-error{{cannot find type 'Nope' in scope}}

func bothSpellings(_ fn: @MainActor @isolated(MainActor) () -> Void) {} // expected-error{{function type cannot be isolated with '@isolated(...)' and global actor attribute @MainActor at the same time}}

func withIsolatedAny(_ fn: @isolated(any) @isolated(MainActor) () -> Void) {} // expected-error{{duplicate attribute}}

func withIsolatedParameter<G: GlobalActor>(_: G.Type, _ fn: @isolated(G) (isolated any Actor) -> Void) {}
// expected-minimal-warning@-1{{function type cannot have global actor and 'isolated' parameter; this is an error in the Swift 6 language mode}}
// expected-complete-warning@-2{{function type cannot have global actor and 'isolated' parameter; this is an error in the Swift 6 language mode}}
// expected-swift6-error@-3{{function type cannot have global actor and 'isolated' parameter}}

// Only allowed on function types, like '@G'
let notAFunction: @isolated(MainActor) Int = 0 // expected-error{{'@isolated' only applies to function types}}

// Isolated conformances can be spelled with '@isolated(G)' as well, see
// isolated_global_actor_isolated_conformance.swift, but only for a concrete
// global actor
protocol P {}
struct IsolatedConformance: @isolated(MainActor) P {} // okay

struct GenericIsolatedConformance<G: GlobalActor>: @isolated(G) P {} // expected-error{{conformance cannot be isolated to generic global actor 'G'}}

struct NotGlobalActorIsolatedConformance: @isolated(NotGlobalActor) P {} // expected-error{{type 'NotGlobalActor' in '@isolated' attribute is not a global actor}}

struct BothSpellingsIsolatedConformance: @MainActor @isolated(MainActor) P {} // expected-error{{function type cannot be isolated with '@isolated(...)' and global actor attribute @MainActor at the same time}}

struct ErasedIsolatedConformance: @isolated(any) P {} // expected-error{{'@isolated' only applies to function types}}

func closureAttribute() {
  // Closure attributes are declaration attributes, and 'isolated' is a
  // declaration modifier
  _ = { @isolated(MainActor) in }
  // expected-error@-1{{'isolated' is a declaration modifier, not an attribute}}
  // expected-error@-2{{cannot infer type of closure parameter 'MainActor' without a type annotation}}
}
