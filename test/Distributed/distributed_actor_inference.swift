// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-distributed -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed
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
  distributed func hello()  // ok
}

protocol DPOK2: DPOK {
  distributed func again()  // ok
}

enum SomeNotActorEnum_5 {
  distributed func nopeAsyncThrows() async throws -> Int { 42 } // expected-error{{'distributed' method can only be declared within 'distributed actor'}}
}

distributed actor SomeDistributedActor_6 {
  distributed func yay() async throws -> Int { 42 } // ok
}

distributed actor SomeDistributedActor_7 {
  distributed func dont_1() async throws -> Int { 42 } // expected-error{{distributed instance method's 'dont_1' remote counterpart '_remote_dont_1' cannot not be implemented manually.}}
  distributed func dont_2() async throws -> Int { 42 } // expected-error{{distributed instance method's 'dont_2' remote counterpart '_remote_dont_2' cannot not be implemented manually.}}
  distributed func dont_3() async throws -> Int { 42 } // expected-error{{distributed instance method's 'dont_3' remote counterpart '_remote_dont_3' cannot not be implemented manually.}}
}

extension SomeDistributedActor_7 {

  // TODO: we should diagnose a bit more precisely here

  static func _remote_dont_1(actor: SomeDistributedActor_6) async throws -> Int {
    fatalError()
  }
  static func _remote_dont_2(actor: SomeDistributedActor_6) -> Int {
    fatalError()
  }
  static func _remote_dont_3(actor: SomeDistributedActor_6) -> Int {
    fatalError()
  }

  func _remote_dont_3(actor: SomeDistributedActor_6) -> Int {
    fatalError()
  }
  func _remote_dont_4() -> Int {
    fatalError()
  }
}

distributed actor BadValuesDistributedActor_7 {
  distributed var varItNope: Int { 13 } // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed let letItNope: Int = 13 // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed lazy var lazyVarNope: Int = 13 // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed subscript(nope: Int) -> Int { nope * 2 } // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed static let staticLetNope: Int = 13 // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed static var staticVarNope: Int { 13 } // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed static func staticNope() async throws -> Int { 13 } // expected-error{{'distributed' method cannot be 'static'}}
}

