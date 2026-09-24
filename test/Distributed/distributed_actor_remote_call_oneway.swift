// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature OnewayMethods -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_OnewayMethods

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== ------------------------------------------------------------------------
// MARK: Positive cases: 'oneway' modifier on Void-returning distributed funcs

distributed actor Greeter {
  // Synchronous Void func is accepted.
  distributed func thanks() oneway {}

  // Async Void func is accepted; 'oneway' follows the effect specifiers.
  distributed func ping() async oneway {}

  // Explicit '-> Void' is accepted.
  distributed func ack() oneway -> Void {}
}

// ==== ------------------------------------------------------------------------
// MARK: 'oneway' is permitted (but inert) on plain actor instance methods

actor PlainActor {
  func heartbeat() oneway {}
  func tick() async oneway {}
}

// A non-'distributed' method of a distributed actor is still an actor instance
// method, so 'oneway' is permitted (and inert) there too.
distributed actor MixedGreeter {
  func localOnly() oneway {}
}

// ==== ------------------------------------------------------------------------
// MARK: Negative cases: 'oneway' rejects non-Void return

distributed actor NonVoidGreeter {
  // expected-error@+1{{'oneway' instance method 'ohai()' must return 'Void'}}
  distributed func ohai() oneway -> Int { 0 }

  // expected-error@+1{{'oneway' instance method 'greet(name:)' must return 'Void'}}
  distributed func greet(name: String) async oneway -> String { name }
}

// ==== ------------------------------------------------------------------------
// MARK: Negative cases: 'oneway' requires a 'distributed' method or actor method

struct NotAnActor {
  // expected-error@+1{{'oneway' can only be applied to a 'distributed' method or an actor instance method; instance method 'plain()' is neither}}
  func plain() oneway {}
}

func freeFunction() oneway {}
// expected-error@-1{{'oneway' can only be applied to a 'distributed' method or an actor instance method; global function 'freeFunction()' is neither}}
