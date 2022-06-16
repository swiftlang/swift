// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== ------------------------------------------------------------------------
// MARK: Protocols

protocol LocalProto {
  func local()
  // expected-note@-1 2{{mark the protocol requirement 'local()' 'async throws' to allow actor-isolated conformances}}

  func localAsync() async
  // expected-note@-1 2{{mark the protocol requirement 'localAsync()' 'throws' to allow actor-isolated conformances}}{{26-26= throws}}

  func localThrows() throws
  // expected-note@-1 2{{mark the protocol requirement 'localThrows()' 'async' to allow actor-isolated conformances}}{{22-22=async }}

  func localAsyncThrows() async throws
  // expected-note@-1{{'localAsyncThrows()' declared here}}
}

// TODO(distributed): It should be possible for a distributed actor to conform to this protocol (!),
//    along with known to be local and proper checking, this can be made safe, and then we have full
//    protocol oriented programming capabilities for our actors, without having to suddenly
//    expose everything as distributed (!); rdar://84587437
protocol MixedProtoDistributedActor: DistributedActor {
  // only callable on a "known to be local" distributed actor:
  func local()
  func localAsync() async
  func localThrows() throws
  func localAsyncThrows() async throws

  distributed func dist()
  distributed func distAsync() async
  distributed func distAsyncThrows() async throws
}

protocol DistProtoDistributedActor: DistributedActor {
  distributed func dist()
  distributed func distAsync() async
  distributed func distThrows() throws
  distributed func distAsyncThrows() async throws
}

// ==== ------------------------------------------------------------------------
// MARK: Actors

distributed actor DAL: LocalProto {
  func local() {}
  // expected-error@-1{{distributed actor-isolated instance method 'local()' cannot be used to satisfy nonisolated protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'local()' to make this instance method not isolated to the actor}}
  func localAsync() async {}
  // expected-error@-1{{distributed actor-isolated instance method 'localAsync()' cannot be used to satisfy nonisolated protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'localAsync()' to make this instance method not isolated to the actor}}
  func localThrows() throws {}
  // expected-error@-1{{distributed actor-isolated instance method 'localThrows()' cannot be used to satisfy nonisolated protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'localThrows()' to make this instance method not isolated to the actor}}
  func localAsyncThrows() async throws {}
  // expected-error@-1{{distributed actor-isolated instance method 'localAsyncThrows()' cannot be used to satisfy nonisolated protocol requirement}}
  // expected-note@-2{{add 'nonisolated' to 'localAsyncThrows()' to make this instance method not isolated to the actor}}
  // expected-note@-3{{add 'distributed' to 'localAsyncThrows()' to make this instance method satisfy the protocol requirement}}
}

distributed actor DAD: LocalProto {
  distributed func local() {}
  // expected-error@-1{{actor-isolated distributed instance method 'local()' cannot be used to satisfy nonisolated protocol requirement}}

  distributed func localAsync() async {}
  // expected-error@-1{{actor-isolated distributed instance method 'localAsync()' cannot be used to satisfy nonisolated protocol requirement}}

  distributed func localThrows() throws {}
  // expected-error@-1{{actor-isolated distributed instance method 'localThrows()' cannot be used to satisfy nonisolated protocol requirement}}

  distributed func localAsyncThrows() async throws {} // ok!
}

// ==== ------------------------------------------------------------------------

distributed actor DA2: DistProtoDistributedActor {
  func local() {}
  // expected-note@-1{{distributed actor-isolated instance method 'local()' declared here}}
  func localAsync() async {}
  // expected-note@-1{{distributed actor-isolated instance method 'localAsync()' declared here}}
  func localAsyncThrows() async throws {}
  // expected-note@-1{{distributed actor-isolated instance method 'localAsyncThrows()' declared here}}

  distributed func dist() {}
  distributed func distAsync() async {}
  distributed func distThrows() throws {}
  distributed func distAsyncThrows() async throws {}
}

func testDA2(da: DA2) async throws {
  da.local() // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}
  await da.localAsync() // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}
  try await da.localAsyncThrows() // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}

  try await da.dist()
  try await da.distAsync()
  try await da.distThrows()
  try await da.distAsyncThrows()
}

func testDA2_butKnownToBeLocal(da: DA2) async throws {
  try await da.whenLocal { __secretlyKnownToBeLocal in
    __secretlyKnownToBeLocal.local()
    await __secretlyKnownToBeLocal.localAsync()
    try await __secretlyKnownToBeLocal.localAsyncThrows()

    __secretlyKnownToBeLocal.dist()
    await __secretlyKnownToBeLocal.distAsync()
    try __secretlyKnownToBeLocal.distThrows()
    try await __secretlyKnownToBeLocal.distAsyncThrows()
  }
}
