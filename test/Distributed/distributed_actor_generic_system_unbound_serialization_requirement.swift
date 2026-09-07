// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -target %target-swift-5.7-abi-triple %s
// REQUIRES: concurrency
// REQUIRES: distributed

// A distributed actor may be generic over an actor system, but as soon as a
// distributed function needs to check parameter/return types, we need to know
// the specific SerializationRequirement.

import Distributed

// ==== -----------------------------------------------------------------------
// MARK: Inheritance clause spelling

distributed actor Unbound<ActorSystem: DistributedActorSystem> {
  // expected-note@-1 3{{constrain 'ActorSystem' with concrete actor system or 'SerializationRequirement', e.g. 'DistributedActorSystem<any Codable>'}}{{62-62=<any Codable>}}

  distributed func param(x: Int) {}
  // expected-error@-1{{distributed instance method 'param(x:)' cannot be checked because actor system 'ActorSystem' does not specify a concrete 'SerializationRequirement'}}

  distributed func result() -> Int { 0 }
  // expected-error@-1{{distributed instance method 'result()' cannot be checked because actor system 'ActorSystem' does not specify a concrete 'SerializationRequirement'}}

  distributed var value: Int { 0 }
  // expected-error@-1{{distributed property 'value' cannot be checked because actor system 'ActorSystem' does not specify a concrete 'SerializationRequirement'}}

  // Nothing has to cross the wire here, so this stays legal:
  distributed func nothingToSerialize() {}
}

distributed actor UnboundGenericFunc<ActorSystem: DistributedActorSystem> {
  // expected-note@-1{{constrain 'ActorSystem' with concrete actor system or 'SerializationRequirement', e.g. 'DistributedActorSystem<any Codable>'}}{{73-73=<any Codable>}}

  distributed func generic<T: Codable>(x: T) {}
  // expected-error@-1{{distributed instance method 'generic(x:)' cannot be checked because actor system 'ActorSystem' does not specify a concrete 'SerializationRequirement'}}
}

// ==== -----------------------------------------------------------------------
// MARK: 'where' clause spelling

distributed actor UnboundWhereClause<AS> where AS: DistributedActorSystem {
  // expected-note@-1{{constrain 'AS' with concrete actor system or 'SerializationRequirement', e.g. 'DistributedActorSystem<any Codable>'}}{{74-74=<any Codable>}}
  typealias ActorSystem = AS

  distributed func param(x: Int) {}
  // expected-error@-1{{distributed instance method 'param(x:)' cannot be checked because actor system 'UnboundWhereClause<AS>.ActorSystem' (aka 'AS') does not specify a concrete 'SerializationRequirement'}}
}

// ==== -----------------------------------------------------------------------
// MARK: Distributed targets declared in an extension
//
// The note goes on the extension rather than the actor: the actor may live in
// another file or module, and may legitimately stay generic over its system.

distributed actor UnboundExtended<ActorSystem: DistributedActorSystem> {}

extension UnboundExtended {
  // expected-note@-1{{constrain this extension with concrete actor system or 'SerializationRequirement', e.g. 'DistributedActorSystem<any Codable>'}}{{27-27= where ActorSystem: DistributedActorSystem<any Codable> }}
  distributed func param(x: Int) {}
  // expected-error@-1{{distributed instance method 'param(x:)' cannot be checked because actor system 'ActorSystem' does not specify a concrete 'SerializationRequirement'}}
}

protocol Marker {}

// An existing 'where' clause is appended to instead.
extension UnboundExtended where ActorSystem: Marker {
  // expected-note@-1{{constrain this extension with concrete actor system or 'SerializationRequirement', e.g. 'DistributedActorSystem<any Codable>'}}{{52-52=, ActorSystem: DistributedActorSystem<any Codable>}}
  distributed func other(x: Int) {}
  // expected-error@-1{{distributed instance method 'other(x:)' cannot be checked because actor system 'ActorSystem' does not specify a concrete 'SerializationRequirement'}}
}

// ==== -----------------------------------------------------------------------
// MARK: Pinning the requirement makes all of the above legal

distributed actor Bound<ActorSystem: DistributedActorSystem<any Codable>> {
  distributed func param(x: Int) {}
  distributed func result() -> Int { 0 }
  distributed var value: Int { 0 }
  distributed func generic<T: Codable>(x: T) -> T { x }
}

distributed actor BoundWhereClause<ActorSystem> where ActorSystem: DistributedActorSystem<any Codable> {
  distributed func param(x: Int) {}
  distributed func result() -> Int { 0 }
}
