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
open class GA2 { // expected-error{{global actor 'GA2' requires a static property 'shared' that produces an actor instance}}{{17-17=\n    public static let shared = <#actor instance#>}}
}

@globalActor
struct GA3 { // expected-error{{global actor 'GA3' requires a static property 'shared' that produces an actor instance}}
  let shared = SomeActor() // expected-note{{'shared' property in global actor is not 'static'}}{{3-3=static }}
}

@globalActor
struct GA4 { // expected-error{{global actor 'GA4' requires a static property 'shared' that produces an actor instance}}
  private static let shared = SomeActor() // expected-note{{'shared' property has more restrictive access (private) than its global actor (internal)}}{{3-11=}}
}

@globalActor
open class GA5 { // expected-error{{global actor 'GA5' requires a static property 'shared' that produces an actor instance}}
  static let shared = SomeActor() // expected-note{{'shared' property has more restrictive access (internal) than its global actor (public)}}{{3-3=public}}
}

@globalActor
struct GA6<T> { // expected-error{{global actor 'GA6' requires a static property 'shared' that produces an actor instance}}
}

extension GA6 where T: Equatable {
  static var shared: SomeActor { SomeActor() } // expected-note{{'shared' property in global actor cannot be in a constrained extension}}
}

@globalActor
class GA7 { // expected-error{{global actor 'GA7' requires a static property 'shared' that produces an actor instance}}
  static let shared = 5 // expected-note{{'shared' property type 'Int' does not conform to the 'Actor' protocol}}
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
  @GA1 nonisolated func conflict1() { } // expected-error{{instance method 'conflict1()' has multiple actor-isolation attributes ('nonisolated' and 'GA1')}}
}
