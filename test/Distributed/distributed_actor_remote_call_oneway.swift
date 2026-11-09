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
// MARK: Positive cases: '@oneway' attribute on Void-returning distributed funcs

distributed actor Greeter {
  // Synchronous Void func is accepted
  @oneway distributed func thanks() {}

  // Async Void func is accepted
  @oneway distributed func ping() async {}

  // Explicit '-> Void' is accepted
  @oneway distributed func ack() -> Void {}
}

// ==== -----------------------------------------------------------------------
// MARK: '@oneway' is permitted on plain actor methods

actor PlainActor {
  @oneway func heartbeat() {}
  @oneway func tick() async {}
}

// A non-'distributed' method of a distributed actor is still an actor instance
// method, so '@oneway' is permitted there too
distributed actor MixedGreeter {
  @oneway func localOnly() {}
}

// ==== -----------------------------------------------------------------------
// MARK: '@oneway' is permitted on global-actor-isolated methods

@MainActor
@oneway func refreshUI() {}

@MainActor
class Screen {
  @oneway func repaint() {}
  @oneway func reflow() async {}
}

// ==== -----------------------------------------------------------------------
// MARK: Negative cases: '@oneway' rejects non-Void return

distributed actor NonVoidGreeter {
  // expected-error@+1{{'@oneway' distributed instance method 'ohai()' must return 'Void'}}
  @oneway distributed func ohai() -> Int { 0 }

  // expected-error@+1{{'@oneway' distributed instance method 'greet(name:)' must return 'Void'}}
  @oneway distributed func greet(name: String) async -> String { name }
}

// ==== -----------------------------------------------------------------------
// MARK: Negative cases: '@oneway' rejects 'throws'

distributed actor ThrowingGreeter {
  // expected-error@+1{{'@oneway' distributed instance method 'boom()' must not throw}}
  @oneway distributed func boom() async throws {}
}

actor ThrowingActor {
  // expected-error@+1{{'@oneway' instance method 'oops()' must not throw}}
  @oneway func oops() throws {}
}

// ==== -----------------------------------------------------------------------
// MARK: '@oneway' is not permitted on a function which is not actor isolated

struct NotAnActor {
  @oneway func plain() {}
  // expected-error@-1:3{{'@oneway' can only be applied to an actor isolated method}}
}

@oneway func freeFunction() {}
// expected-error@-1:1{{'@oneway' can only be applied to an actor isolated method}}
