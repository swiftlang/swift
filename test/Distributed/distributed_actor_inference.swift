// RUN: %target-typecheck-verify-swift -enable-experimental-distributed
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

actor SomeActor { }

// ==== ------------------------------------------------------------------------
// MARK: Declaring distributed actors
// GOOD:
@available(SwiftStdlib 5.5, *)
distributed actor SomeDistributedActor_0 { }

// BAD:
@available(SwiftStdlib 5.5, *)
distributed class SomeDistributedActor_1 { } // expected-error{{'distributed' can only be applied to 'actor' definitions, and distributed actor-isolated async functions}}
@available(SwiftStdlib 5.5, *)
distributed struct SomeDistributedActor_2 { } // expected-error{{'distributed' modifier cannot be applied to this declaration}}
@available(SwiftStdlib 5.5, *)
distributed enum SomeDistributedActor_3 { } // expected-error{{'distributed' modifier cannot be applied to this declaration}}

// ==== ------------------------------------------------------------------------
// MARK: Declaring distributed functions
// NOTE: not distributed actor, so cannot have any distributed functions

@available(SwiftStdlib 5.5, *)
struct SomeNotActorStruct_2 {
  distributed func nopeAsyncThrows() async throws -> Int { 42 } // expected-error{{'distributed' function can only be declared within 'distributed actor'}}
}

@available(SwiftStdlib 5.5, *)
class SomeNotActorClass_3 {
  distributed func nopeAsyncThrows() async throws -> Int { 42 } // expected-error{{'distributed' function can only be declared within 'distributed actor'}}
}

@available(SwiftStdlib 5.5, *)
actor SomeNotDistributedActor_4 {
  distributed func notInDistActorAsyncThrowing() async throws -> Int { 42 } // expected-error{{'distributed' function can only be declared within 'distributed actor'}}
}

protocol DP {
  distributed func hello()  // expected-error{{'distributed' function can only be declared within 'distributed actor'}}
}

@available(SwiftStdlib 5.5, *)
protocol DPOK: DistributedActor {
  distributed func hello()  // ok
}

@available(SwiftStdlib 5.5, *)
protocol DPOK2: DPOK {
  distributed func again()  // ok
}

@available(SwiftStdlib 5.5, *)
enum SomeNotActorEnum_5 {
  distributed func nopeAsyncThrows() async throws -> Int { 42 } // expected-error{{'distributed' function can only be declared within 'distributed actor'}}
}

@available(SwiftStdlib 5.5, *)
distributed actor SomeDistributedActor_6 {
  distributed func yay() async throws -> Int { 42 } // ok
}

@available(SwiftStdlib 5.5, *)
extension SomeDistributedActor_6 {
  static func _remote_yay(actor: SomeDistributedActor_6) async throws -> Int {
    fatalError()
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor BadValuesDistributedActor_7 {
  distributed var varItNope: Int { 13 } // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed let letItNope: Int = 13 // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed lazy var lazyVarNope: Int = 13 // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed subscript(nope: Int) -> Int { nope * 2 } // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed static let staticLetNope: Int = 13 // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed static var staticVarNope: Int { 13 } // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed static func staticNope() async throws -> Int { 13 } // expected-error{{'distributed' functions cannot be 'static'}}
}

