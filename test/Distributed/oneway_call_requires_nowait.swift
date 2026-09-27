// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -I %t 2>&1 %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_OnewayNowait

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== -----------------------------------------------------------------------
// MARK: 'nowait' is a general keyword, independent of 'oneway'

actor Counter {
  func increment() {}
}

func testGeneralNowait(_ c: Counter) {
  // A plain (non-'oneway') Void, non-throwing call may use 'nowait' too
  nowait c.increment()
}

// ==== -----------------------------------------------------------------------
// MARK: 'nowait' is mandatory for 'oneway' calls: plain actor methods

actor Worker {
  func heartbeat() oneway {}
  func tick() async oneway {}

  func selfCalls() async {
    // Accepted: wrapped in 'nowait'. 'nowait' unconditionally wraps its
    // operand in an implicit 'await', which is a no-op for this synchronous
    // self-call, hence the secondary warning.
    // expected-warning@+1{{no 'async' operations occur within 'await' expression}}
    nowait self.heartbeat()
    nowait self.tick()

    // Rejected: plain call, no 'nowait'.
    // expected-error@+1{{call to 'oneway' instance method 'heartbeat()' must use 'nowait'}}
    self.heartbeat()

    // Rejected: explicit 'await' without 'nowait'.
    // expected-error@+1{{call to 'oneway' instance method 'tick()' must use 'nowait'}}
    await self.tick()
  }
}

// ==== -----------------------------------------------------------------------
// MARK: 'nowait' is mandatory for 'oneway' calls: distributed methods

distributed actor Greeter {
  distributed func thanks() oneway {}
  distributed func ping() async oneway {}
}

func testDistributedOneway(_ g: Greeter) async throws {
  // Accepted: wrapped in 'nowait'
  nowait g.thanks()
  nowait g.ping()

  // Rejected: a distributed call always needs explicit 'try await', but that
  // alone is not 'nowait'.
  // expected-error@+1{{call to 'oneway' distributed instance method 'ping()' must use 'nowait'}}
  try await g.ping()
}
