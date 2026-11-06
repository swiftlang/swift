// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t 2>&1 %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== -----------------------------------------------------------------------
// MARK: 'send' is mandatory for '@oneway' calls: distributed methods

distributed actor Greeter {
  @oneway distributed func thanks() {}
  @oneway distributed func ping() async {}
}

func testDistributedOneway(_ g: Greeter) async throws {
  // Accepted: wrapped in 'send', which calls the synchronous, throwing
  // distributed thunk
  try send g.thanks()
  try send g.ping()

  // Rejected: the distributed thunk throws
  // expected-error@+1{{call can throw but is not marked with 'try'}}
  send g.thanks()
  // expected-note@-1{{did you mean to use 'try'?}}
  // expected-note@-2{{did you mean to handle error as optional value?}}
  // expected-note@-3{{did you mean to disable error propagation?}}

  // Warned: a distributed call needs explicit 'try await', which makes an
  // ordinary call of the synchronous distributed thunk
  // expected-warning@+1{{'@oneway' distributed instance method 'ping()' is not intended to be awaited on, use 'send' instead}}{{7-12=send}}
  try await g.ping()
}
