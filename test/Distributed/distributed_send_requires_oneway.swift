// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -plugin-path %swift-plugin-dir -I %t %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SendCalls

// 'send' of a call which goes through a distributed thunk, i.e. on a
// distributed actor which may be remote, requires the function to be
// '@oneway'. A call on a known-local actor does not go through a
// distributed thunk and is sent like any other actor call

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== -----------------------------------------------------------------------
// MARK: Concrete distributed actor

distributed actor Worker {
  distributed func pause() {}
  // expected-note@-1 3{{mark distributed instance method 'pause()' '@oneway' to call it with 'send'}}{{3-3=@oneway }}

  @oneway distributed func heartbeat() {}

  // A call on the isolated 'self' is local, it does not go through the
  // distributed thunk
  func selfSend() {
    send self.pause()
    send pause()
  }

  distributed func distributedSelfSend() {
    send self.pause()
  }

  func selfSendAsync() async throws {
    send self.pause()
    send self.heartbeat()
  }
}

func testConcrete(_ w: Worker) async throws {
  send w.pause()
  // expected-error@-1{{'send' of distributed instance method 'pause()' requires it to be '@oneway'}}
  // expected-note@-2{{use 'try await' to call distributed instance method 'pause()' and wait for its reply}}{{3-7=try await}}

  // OK: '@oneway'
  try send w.heartbeat()

  // OK: waits for the reply
  try await w.pause()
}

// No 'try await' in a synchronous context
func testConcreteSync(_ w: Worker) throws {
  send w.pause()
  // expected-error@-1{{'send' of distributed instance method 'pause()' requires it to be '@oneway'}}

  try send w.heartbeat()
}

// 'await' cannot be combined with 'send' of a distributed function either
func testAwaitSend(_ w: Worker) async throws {
  // expected-error@+1:7{{'await' cannot be combined with 'send'}}{{7-13=}}
  try await send w.heartbeat()
}

func testOptionalChain(_ w: Worker?) async throws {
  send w?.pause()
  // expected-error@-1{{'send' of distributed instance method 'pause()' requires it to be '@oneway'}}
  // expected-note@-2{{use 'try await' to call distributed instance method 'pause()' and wait for its reply}}{{3-7=try await}}
}

// ==== -----------------------------------------------------------------------
// MARK: Generic actor system

distributed actor GenericWorker<S: DistributedActorSystem<any Codable>> {
  typealias ActorSystem = S

  distributed func pause() {}
  // expected-note@-1{{mark distributed instance method 'pause()' '@oneway' to call it with 'send'}}{{3-3=@oneway }}
}

func testGenericSystem<S>(_ w: GenericWorker<S>) async throws {
  send w.pause()
  // expected-error@-1{{'send' of distributed instance method 'pause()' requires it to be '@oneway'}}
  // expected-note@-2{{use 'try await' to call distributed instance method 'pause()' and wait for its reply}}{{3-7=try await}}

  try await w.pause()
}

// ==== -----------------------------------------------------------------------
// MARK: Protocol requirements

protocol Pausable: DistributedActor where ActorSystem == FakeActorSystem {
  distributed func pause()
  // expected-note@-1 3{{mark distributed instance method 'pause()' '@oneway' to call it with 'send'}}{{3-3=@oneway }}

  @oneway distributed func heartbeat()
}

func testAny(_ p: any Pausable) async throws {
  send p.pause()
  // expected-error@-1{{'send' of distributed instance method 'pause()' requires it to be '@oneway'}}
  // expected-note@-2{{use 'try await' to call distributed instance method 'pause()' and wait for its reply}}{{3-7=try await}}

  try send p.heartbeat()
  try await p.pause()
}

func testSome(_ p: some Pausable) async throws {
  send p.pause()
  // expected-error@-1{{'send' of distributed instance method 'pause()' requires it to be '@oneway'}}
  // expected-note@-2{{use 'try await' to call distributed instance method 'pause()' and wait for its reply}}{{3-7=try await}}

  try send p.heartbeat()
}

func testGeneric<P: Pausable>(_ p: P) async throws {
  send p.pause()
  // expected-error@-1{{'send' of distributed instance method 'pause()' requires it to be '@oneway'}}
  // expected-note@-2{{use 'try await' to call distributed instance method 'pause()' and wait for its reply}}{{3-7=try await}}

  try send p.heartbeat()
}

// ==== -----------------------------------------------------------------------
// MARK: '@Resolvable' protocols

// The stub is declared by the macro, the requirement is the one to mark
@Resolvable
protocol Service: DistributedActor where ActorSystem == FakeActorSystem {
  distributed func stop()
  // expected-note@-1{{mark distributed instance method 'stop()' '@oneway' to call it with 'send'}}{{3-3=@oneway }}

  @oneway distributed func heartbeat()
}

func testResolvable(_ s: $Service) async throws {
  send s.stop()
  // expected-error@-1{{'send' of distributed instance method 'stop()' requires it to be '@oneway'}}
  // expected-note@-2{{use 'try await' to call distributed instance method 'stop()' and wait for its reply}}{{3-7=try await}}

  try send s.heartbeat()
  try await s.stop()
}
