// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

actor SomeActor { }

// ==== ------------------------------------------------------------------------
// MARK: Declaring distributed actors
// GOOD:
distributed actor SomeDistributedActor_0 { }

// BAD:
distributed class SomeDistributedActor_1 { } // expected-error{{'distributed' can only be applied to 'actor' definitions, and distributed actor-isolated async functions}}
distributed struct SomeDistributedActor_2 { } // expected-error{{'distributed' modifier cannot be applied to this declaration}}
distributed enum SomeDistributedActor_3 { } // expected-error{{'distributed' modifier cannot be applied to this declaration}}

// ==== ------------------------------------------------------------------------
// MARK: Declaring distributed functions
// NOTE: not distributed actor, so cannot have any distributed functions

struct SomeNotActorStruct_2 {
  distributed func nopeAsyncThrows() async throws -> Int { 42 } // expected-error{{'distributed' method can only be declared within 'distributed actor'}}
}

class SomeNotActorClass_3 {
  distributed func nopeAsyncThrows() async throws -> Int { 42 } // expected-error{{'distributed' method can only be declared within 'distributed actor'}}
}

actor SomeNotDistributedActor_4 {
  distributed func notInDistActorAsyncThrowing() async throws -> Int { 42 } // expected-error{{'distributed' method can only be declared within 'distributed actor'}}
}

protocol DP {
  distributed func hello()  // expected-error{{'distributed' method can only be declared within 'distributed actor'}}
}

protocol DPOK: DistributedActor {
  distributed func hello()
}

protocol DPOK2: DPOK {
  distributed func again()
}

enum SomeNotActorEnum_5 {
  distributed func nopeAsyncThrows() async throws -> Int { 42 } // expected-error{{'distributed' method can only be declared within 'distributed actor'}}
}

distributed actor SomeDistributedActor_6 {
  distributed func yay() async throws -> Int { 42 } // ok
}

distributed actor BadValuesDistributedActor_7 {
  distributed var varItNope: Int { 13 } // we allow these
  distributed let letItNope: Int = 13 // expected-error{{property 'letItNope' cannot be 'distributed', only computed properties can}}
  distributed lazy var lazyVarNope: Int = 13 // expected-error{{property 'lazyVarNope' cannot be 'distributed', only computed properties can}}
  distributed subscript(nope: Int) -> Int { nope * 2 } // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed static let staticLetNope: Int = 13 // expected-error{{'distributed' property 'staticLetNope' cannot be 'static'}}
  distributed static var staticVarNope: Int { 13 } // expected-error{{'distributed' property 'staticVarNope' cannot be 'static'}}
  distributed static func staticNope() async throws -> Int { 13 } // expected-error{{'distributed' method cannot be 'static'}}
}

