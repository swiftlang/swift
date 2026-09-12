// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed

import _Concurrency
import Distributed

// ==== ----------------------------------------------------------------------
// MARK: A distributed actor cannot be generic over its actor system

// Embedded is monomorphized, and a generic actor like this would prevent
// monomorphization of the encode calls in generated thunks.

// expected-error@+1{{distributed actor cannot be generic over its actor system in Embedded Swift; specify a concrete 'ActorSystem'}}
distributed actor GenericEmpty<ActorSystem> where ActorSystem: DistributedActorSystem {
}

// ==== ----------------------------------------------------------------------
// MARK: 'distributed var' is not supported

// No inherent limitation, we just don't do it for now.

distributed actor DistributedVarActor {
  typealias ActorSystem = EmbeddedFakeRoundtripActorSystem

  // expected-error@+1{{'distributed' computed properties are not supported in Embedded Swift; use a 'distributed func' instead}}
  distributed var name: String {
    "Kappa"
  }

  distributed func hello(name: String) -> String {
    return "Hello, \(name)!"
  }
}

// ==== ----------------------------------------------------------------------
// MARK: Argument / return types must conform to the SerializationRequirement

// Same mechanism as non-Embedded, just making sure it triggers in Embedded as well.

struct NotSerializable: Sendable {}

distributed actor ConformanceActor {
  typealias ActorSystem = EmbeddedFakeRoundtripActorSystem

  // All argument/return types conform, so this func compiles fine.
  distributed func ok(name: String) -> String {
    return "Hello, \(name)!"
  }

  // Non-conforming parameter is rejected.
  // expected-error@+1{{parameter 'value' of type 'NotSerializable' in distributed instance method does not conform to serialization requirement 'EmbeddedSerializationRequirement'}}
  distributed func take(value: NotSerializable) {
  }

  // Conforming parameter, but a non-conforming result -> the result is rejected.
  // expected-error@+1{{result type 'NotSerializable' of distributed instance method 'make' does not conform to serialization requirement 'EmbeddedSerializationRequirement'}}
  distributed func make(from s: String) -> NotSerializable {
    return NotSerializable()
  }
}
