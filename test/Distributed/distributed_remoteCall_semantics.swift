// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature DistributedRemoteCallSemantics -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_DistributedRemoteCallSemantics

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor Greeter {
  @remoteCall(oneway)
  distributed func thanks() {}

  @remoteCall(oneway)
  distributed func ping() async {}

  @remoteCall(oneway)
  distributed func ack() -> Void {}

  @remoteCall(oneway)
  // expected-warning@+1{{remote call semantic 'oneway' specified more than once has no additional effect}}
  @remoteCall(oneway)
  distributed func stacked() {}

  // expected-error@+1{{'@remoteCall(oneway)' distributed instance method 'ohai()' must return 'Void'}}
  @remoteCall(oneway)
  distributed func ohai() -> Int { 0 }

  // expected-error@+1{{'@remoteCall(oneway)' distributed instance method 'greet(name:)' must return 'Void'}}
  @remoteCall(oneway)
  distributed func greet(name: String) async -> String { name }

  // expected-error@+1{{'@remoteCall(oneway)' cannot be applied to properties; use a method instead}}
  @remoteCall(oneway)
  distributed var status: Int { 0 }
}

distributed actor FixItGreeter {
  // expected-error@+1{{'@remoteCall(oneway)' distributed instance method 'asyncRet()' must return 'Void'}}{{+1:37-+1:47=}}
  @remoteCall(oneway)
  distributed func asyncRet() async -> String { "" }

  // expected-error@+1{{'@remoteCall(oneway)' distributed instance method 'bothEffects()' must return 'Void'}}{{+1:47-+1:54=}}
  @remoteCall(oneway)
  distributed func bothEffects() async throws -> Int { 0 }
}

// ==== ------------------------------------------------------------------------
// MARK: Good: @remoteCall(blocking) on funcs and computed properties

distributed actor BlockingActor {
  // 'blocking' accepts a non-Void, possibly async, method.
  @remoteCall(blocking)
  distributed func hello(name: String) async -> Int { 0 }

  @remoteCall(blocking)
  distributed func ping() async {}

  // Unlike 'oneway', 'blocking' is allowed on a computed property.
  @remoteCall(blocking)
  distributed var status: Int { 0 }
}

func callBlockingActor(_ a: BlockingActor) async throws {
  let _: Int = try await a.hello(name: "a")
  _ = try await a.status
}

// ==== ------------------------------------------------------------------------
// MARK: Bad: @remoteCall(oneway) requires a 'distributed' member

struct NotAnActor {
  // expected-error@+1{{'@remoteCall' can only be applied to 'distributed' methods and computed properties}}
  @remoteCall(oneway)
  func plain() {}
}

distributed actor MixedGreeter {
  // expected-error@+1{{'@remoteCall' can only be applied to 'distributed' methods and computed properties}}
  @remoteCall(oneway)
  func localOnly() {}

  // expected-error@+1{{'@remoteCall' can only be applied to 'distributed' methods and computed properties}}
  @remoteCall(blocking)
  var localOnlyStatus: Int { 0 }
}

// ==== ------------------------------------------------------------------------
// MARK: Bad: @remoteCall on an accessor must move to the property

distributed actor AccessorGreeter {
  distributed var oneway: Int {
    // expected-error@+1{{'@remoteCall' cannot be applied to an accessor; apply it to the property declaration instead}}
    @remoteCall(oneway)
    get { 0 }
  }

  distributed var blocking: Int {
    // expected-error@+1{{'@remoteCall' cannot be applied to an accessor; apply it to the property declaration instead}}
    @remoteCall(blocking)
    get { 0 }
  }
}

// ==== ------------------------------------------------------------------------
// MARK: Bad: 'oneway' + 'blocking' is an illegal combination

distributed actor Contradictory {
  // expected-error@+1{{illegal combination of remote call semantics: a 'oneway' remote call cannot also be 'blocking'}}
  @remoteCall(oneway)
  @remoteCall(blocking)
  distributed func confused() {}
}

// ==== ------------------------------------------------------------------------
// MARK: Good: Inheriting remote call semantics

protocol InheritingGreeter: DistributedActor
    where ActorSystem: DistributedActorSystem<any Codable> {
  @remoteCall(oneway)
  distributed func notify(name: String)
}

// The witness inherits '@remoteCall(oneway)' from the protocol requirement
distributed actor SilentGreeter: InheritingGreeter {
  distributed func notify(name: String) {}
}

// Restating the same semantic on the witness is allowed and does not warn.
distributed actor LoudGreeter: InheritingGreeter {
  @remoteCall(oneway)
  distributed func notify(name: String) {}
}

// ==== ------------------------------------------------------------------------
// MARK: Bad: Conflicting semantics inherited from different protocols

protocol OnewayGreeter: DistributedActor
    where ActorSystem: DistributedActorSystem<any Codable> {
  @remoteCall(oneway) // expected-note{{remote call 'oneway' semantics declared here}}
  distributed func thanks(name: String)
}

protocol BlockingGreeter: DistributedActor
    where ActorSystem: DistributedActorSystem<any Codable> {
  @remoteCall(blocking) // expected-note{{remote call 'blocking' semantics declared here}}
  distributed func thanks(name: String)
}

distributed actor OhNo: OnewayGreeter, BlockingGreeter {
  // The single witness inherits 'oneway' from one protocol and 'blocking' from
  // the other, which is an illegal combination.
  // expected-error@+1{{illegal combination of remote call semantics: a 'oneway' remote call cannot also be 'blocking'}}
  distributed func thanks(name: String) {}
}

