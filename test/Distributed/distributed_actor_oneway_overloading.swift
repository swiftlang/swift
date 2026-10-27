// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls

// '@oneway' is not part of the function type or the mangled name, so
// distributed functions which only differ in '@oneway' redeclare each other,
// just like other functions. Overloads which also differ in other ways are
// fine, and the operand of a 'send' prefers the '@oneway' one

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

// ==== -----------------------------------------------------------------------
// MARK: Redeclarations

distributed actor Greeter {
  distributed func hello() {} // expected-note@:20{{'hello()' previously declared here}}
  @oneway distributed func hello() {}
  // expected-error@-1:28{{invalid redeclaration of 'hello()'}}
}

protocol Speaker: DistributedActor where ActorSystem == FakeActorSystem {
  distributed func speak() // expected-note@:20{{'speak()' previously declared here}}
  @oneway distributed func speak()
  // expected-error@-1:28{{invalid redeclaration of 'speak()'}}
}

// ==== -----------------------------------------------------------------------
// MARK: Ranking

distributed actor Teller {
  @oneway distributed func tell(_ n: Int, priority: Int = 0) {}
  distributed func tell(_ n: Int) {}
}

// The 'send' picks the '@oneway' overload and the 'await' the other one,
// otherwise the 'send' would be an error and the 'await' a warning
func tells(_ t: Teller) async throws {
  try send t.tell(1)
  try await t.tell(2)
}
