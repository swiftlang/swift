// RUN: %target-swift-frontend -typecheck -verify %s -enable-experimental-concurrency
// REQUIRES: concurrency

actor SomeActor { }

// -----------------------------------------------------------------------
// @globalActor attribute itself.
// -----------------------------------------------------------------------

// Well-formed global actor.
@globalActor
struct GA1 {
  static let shared = SomeActor()
}

@globalActor
struct GenericGlobalActor<T> {
  static var shared: SomeActor { SomeActor() }
}

// Ill-formed global actors.
@globalActor
final class GA2 { // expected-error{{type 'GA2' does not conform to protocol 'GlobalActor'}}
}

@globalActor
struct GA3 { // expected-error{{type 'GA3' does not conform to protocol 'GlobalActor'}}
  let shared = SomeActor()
}

@globalActor
struct GA4 {
  private static let shared = SomeActor() // expected-error{{property 'shared' must be as accessible as its enclosing type because it matches a requirement in protocol 'GlobalActor'}}
  // expected-note@-1{{mark the static property as 'internal' to satisfy the requirement}}
}

@globalActor
open class GA5 { // expected-error{{non-final class 'GA5' cannot be a global actor}}
  static let shared = SomeActor() // expected-error{{property 'shared' must be declared public because it matches a requirement in public protocol 'GlobalActor'}}
  // expected-note@-1{{mark the static property as 'public' to satisfy the requirement}}
}

@globalActor
struct GA6<T> { // expected-error{{type 'GA6<T>' does not conform to protocol 'GlobalActor'}}
}

extension GA6 where T: Equatable {
  static var shared: SomeActor { SomeActor() }
}

@globalActor
final class GA7 { // expected-error{{type 'GA7' does not conform to protocol 'GlobalActor'}}
  static let shared = 5 // expected-note{{candidate would match and infer 'ActorType' = 'Int' if 'Int' conformed to 'Actor'}}
}

// -----------------------------------------------------------------------
// Applying global actors to entities.
// -----------------------------------------------------------------------
@globalActor
struct OtherGlobalActor {
  static let shared = SomeActor()
}

@GA1 func f() {
  @GA1 let x = 17 // expected-error{{local variable 'x' cannot have a global actor}}
  _ = x
}

@GA1 struct X {
  @GA1 var member: Int
}

struct Y {
  @GA1 subscript(i: Int) -> Int { i }
}

@GA1 extension Y { }

@GA1 func g() { }

class SomeClass {
  @GA1 init() { }
  @GA1 deinit { } // expected-error{{deinitializer cannot have a global actor}}
}

@GA1 typealias Integer = Int // expected-error{{type alias cannot have a global actor}}

@GA1 actor ActorInTooManyPlaces { } // expected-error{{actor 'ActorInTooManyPlaces' cannot have a global actor}}

@GA1 @OtherGlobalActor func twoGlobalActors() { } // expected-error{{declaration can not have multiple global actor attributes ('OtherGlobalActor' and 'GA1')}}

struct Container {
  // FIXME: Diagnostic could be improved to show the generic arguments.
@GenericGlobalActor<Int> @GenericGlobalActor<String> func twoGenericGlobalActors() { } // expected-error{{declaration can not have multiple global actor attributes ('GenericGlobalActor' and 'GenericGlobalActor')}}
}

// -----------------------------------------------------------------------
// Redundant attributes
// -----------------------------------------------------------------------
extension SomeActor {
  @GA1 nonisolated func conflict1() { } // expected-error 3{{instance method 'conflict1()' has multiple actor-isolation attributes ('nonisolated' and 'GA1')}}
}
