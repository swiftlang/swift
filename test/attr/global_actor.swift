// RUN: %target-swift-frontend -typecheck -verify %s -enable-experimental-concurrency
// REQUIRES: concurrency

import _Concurrency

actor class SomeActor { }

// Well-formed global actor.
@globalActor
struct GA1 {
  static let shared = SomeActor()
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
