// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t %s
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls

// A '@oneway' requirement can be witnessed by a function that is not
// '@oneway' (which has fewer obligations: it can be called without 'send'
// too), through its oneway witness adapter. The reverse does not work: a
// '@oneway' witness would have to be called by 'send', which the callers of a
// non-'@oneway' requirement do not do.

@_spi(OnewaySend) import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// A '@oneway' requirement requires the actor system to be a
// '_DistributedOnewayActorSystem', see distributed_actor_oneway_actor_system.swift
protocol Pinger: DistributedActor where ActorSystem: _DistributedOnewayActorSystem {
  @oneway distributed func ping()
  // expected-note@-1 3{{protocol requires function 'ping()' with type '() -> ()'}}
}

protocol PlainPinger: DistributedActor {
  distributed func ping()
  // expected-note@-1{{protocol requires function 'ping()' with type '() -> ()'}}
}

// A '@oneway' requirement is satisfied by a matching '@oneway' witness
distributed actor GoodPinger: Pinger {
  @oneway distributed func ping() {}
}

// ... and by a witness that is not '@oneway'
distributed actor RelaxedPinger: Pinger {
  distributed func ping() {}
}

// ... also if the witness is in an extension
distributed actor ExtensionPinger {}
extension ExtensionPinger: Pinger {
  distributed func ping() {}
}

// A non-'@oneway' requirement is not satisfied by a '@oneway' witness
distributed actor BadPlainPinger: PlainPinger {
  // expected-error@-1{{type 'BadPlainPinger' does not conform to protocol 'PlainPinger'}}
  // expected-note@-2{{add stubs for conformance}}
  @oneway distributed func ping() {}
  // expected-note@-1{{candidate is '@oneway', but protocol requirement is not}}
}

// A witness cannot have more effects than a '@oneway' requirement
distributed actor ThrowingPinger: Pinger {
  // expected-error@-1{{type 'ThrowingPinger' does not conform to protocol 'Pinger'}}
  // expected-note@-2{{add stubs for conformance}}
  distributed func ping() throws {}
  // expected-note@-1{{candidate throws, but protocol does not allow it}}
}

// A witness cannot be 'async' if a '@oneway' requirement is not
distributed actor AsyncPinger: Pinger {
  // expected-error@-1{{type 'AsyncPinger' does not conform to protocol 'Pinger'}}
  // expected-note@-2{{add stubs for conformance}}
  distributed func ping() async {}
  // expected-note@-1{{candidate is 'async', but protocol requirement is not}}
}

// A '@oneway' requirement is not witnessed by a witness that returns a value
distributed actor ResultPinger: Pinger {
  // expected-error@-1{{type 'ResultPinger' does not conform to protocol 'Pinger'}}
  // expected-note@-2{{add stubs for conformance}}
  distributed func ping() -> Int { 1 }
  // expected-note@-1{{candidate has non-matching type '() -> Int'}}
}

// ==== -----------------------------------------------------------------------
// MARK: Matrix: every requirement form x every witness form

// The requirements:
//   R1: distributed func tell(_ n: Int) async throws
//   R2: distributed func tell(_ n: Int)
//   R3: @oneway distributed func tell(_ n: Int)
//   R4: @oneway distributed func tell(_ n: Int) async
//   R5: @oneway distributed func tell(_ n: Int) async throws (invalid)
// and the witnesses:
//   D1: @oneway distributed func tell(_ n: Int) async
//   D2: @oneway distributed func tell(_ n: Int)
//   D3:         distributed func tell(_ n: Int) async
//   D4:         distributed func tell(_ n: Int)

protocol TellR1: DistributedActor {
  distributed func tell(_ n: Int) async throws
  // expected-note@-1 2{{protocol requires function 'tell' with type '(Int) async throws -> ()'}}
}

protocol TellR2: DistributedActor {
  distributed func tell(_ n: Int)
  // expected-note@-1 3{{protocol requires function 'tell' with type '(Int) -> ()'}}
}

protocol TellR3: DistributedActor where ActorSystem: _DistributedOnewayActorSystem {
  @oneway distributed func tell(_ n: Int)
  // expected-note@-1 2{{protocol requires function 'tell' with type '(Int) -> ()'}}
}

protocol TellR4: DistributedActor where ActorSystem: _DistributedOnewayActorSystem {
  @oneway distributed func tell(_ n: Int) async
}

protocol TellR5: DistributedActor where ActorSystem: _DistributedOnewayActorSystem {
  @oneway distributed func tell(_ n: Int) async throws
  // expected-error@-1{{'@oneway' distributed instance method 'tell' must not throw}}
}

// R1: only the witnesses which are not '@oneway'
distributed actor R1D1: TellR1 {
  // expected-error@-1{{type 'R1D1' does not conform to protocol 'TellR1'}}
  // expected-note@-2{{add stubs for conformance}}
  @oneway distributed func tell(_ n: Int) async {}
  // expected-note@-1{{candidate is '@oneway', but protocol requirement is not}}
}
distributed actor R1D2: TellR1 {
  // expected-error@-1{{type 'R1D2' does not conform to protocol 'TellR1'}}
  // expected-note@-2{{add stubs for conformance}}
  @oneway distributed func tell(_ n: Int) {}
  // expected-note@-1{{candidate is '@oneway', but protocol requirement is not}}
}
distributed actor R1D3: TellR1 {
  distributed func tell(_ n: Int) async {}
}
distributed actor R1D4: TellR1 {
  distributed func tell(_ n: Int) {}
}

// R2: only the synchronous witness which is not '@oneway'
distributed actor R2D1: TellR2 {
  // expected-error@-1{{type 'R2D1' does not conform to protocol 'TellR2'}}
  // expected-note@-2{{add stubs for conformance}}
  @oneway distributed func tell(_ n: Int) async {}
  // expected-note@-1{{candidate is '@oneway', but protocol requirement is not}}
}
distributed actor R2D2: TellR2 {
  // expected-error@-1{{type 'R2D2' does not conform to protocol 'TellR2'}}
  // expected-note@-2{{add stubs for conformance}}
  @oneway distributed func tell(_ n: Int) {}
  // expected-note@-1{{candidate is '@oneway', but protocol requirement is not}}
}
distributed actor R2D3: TellR2 {
  // expected-error@-1{{type 'R2D3' does not conform to protocol 'TellR2'}}
  // expected-note@-2{{add stubs for conformance}}
  distributed func tell(_ n: Int) async {}
  // expected-note@-1{{candidate is 'async', but protocol requirement is not}}
}
distributed actor R2D4: TellR2 {
  distributed func tell(_ n: Int) {}
}

// R3: only the synchronous witnesses
distributed actor R3D1: TellR3 {
  // expected-error@-1{{type 'R3D1' does not conform to protocol 'TellR3'}}
  // expected-note@-2{{add stubs for conformance}}
  @oneway distributed func tell(_ n: Int) async {}
  // expected-note@-1{{candidate is 'async', but protocol requirement is not}}
}
distributed actor R3D2: TellR3 {
  @oneway distributed func tell(_ n: Int) {}
}
distributed actor R3D3: TellR3 {
  // expected-error@-1{{type 'R3D3' does not conform to protocol 'TellR3'}}
  // expected-note@-2{{add stubs for conformance}}
  distributed func tell(_ n: Int) async {}
  // expected-note@-1{{candidate is 'async', but protocol requirement is not}}
}
distributed actor R3D4: TellR3 {
  distributed func tell(_ n: Int) {}
}

// R4: every witness
distributed actor R4D1: TellR4 {
  @oneway distributed func tell(_ n: Int) async {}
}
distributed actor R4D2: TellR4 {
  @oneway distributed func tell(_ n: Int) {}
}
distributed actor R4D3: TellR4 {
  distributed func tell(_ n: Int) async {}
}
distributed actor R4D4: TellR4 {
  distributed func tell(_ n: Int) {}
}

func use(_ p: any Pinger) throws {
  try send p.ping()
}

// ==== -----------------------------------------------------------------------
// MARK: '@oneway' requires actor isolation

protocol IsolatedPinger: DistributedActor {
  // A requirement which is not distributed is isolated to the actor
  @oneway func localPing()
}

distributed actor NonisolatedPinger {
  // expected-error@+1:3{{'@oneway' can only be applied to an actor isolated method}}
  @oneway nonisolated func nonisolatedPing() {}
  // expected-error@+1:3{{'@oneway' can only be applied to an actor isolated method}}
  @oneway static func staticPing() {}
}

func useOptional(_ p: (any Pinger)?) throws {
  try send p?.ping()
}
