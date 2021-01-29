// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency
actor class SomeActor { }

// ==== ------------------------------------------------------------------------
// MARK: Declaring distributed actors
// GOOD:
distributed actor class SomeDistributedActor_0 { }

// BAD:
distributed class SomeDistributedActor_1 { } // expected-error{{'distributed' can only be applied to 'actor class' definitions, and distributed actor-isolated async functions}}
distributed struct SomeDistributedActor_2 { } // expected-error{{'distributed' modifier cannot be applied to this declaration}}
distributed enum SomeDistributedActor_3 { } // expected-error{{'distributed' modifier cannot be applied to this declaration}}
// ==== ------------------------------------------------------------------------
// MARK: Declaring distributed functions
// NOTE: not distributed actor, so cannot have any distributed functions
actor class SomeNotDistributedActor_4 {
  distributed func notInDistActorAsyncThrowing() async throws -> Int { 42 } // expected-error{{'distributed' function can only be declared within 'distributed actor class'}}
}

enum SomeNotActorEnum_5 {
  distributed func nopeAsyncThrows() async throws -> Int { 42 } // expected-error{{'distributed' function can only be declared within 'distributed actor class'}}
}

distributed actor class SomeDistributedActor_6 {
  distributed func yay() async throws -> Int { 42 } // ok
}

distributed actor class BadValuesDistributedActor_7 {
  distributed var varItNope: Int { 13 } // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed let letItNope: Int = 13 // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed lazy var lazyVarNope: Int = 13 // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed subscript(nope: Int) -> Int { nope * 2 } // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed static let staticLetNope: Int = 13 // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed static var staticVarNope: Int { 13 } // expected-error{{'distributed' modifier cannot be applied to this declaration}}
  distributed static func staticNope() async throws -> Int { 13 } // expected-error{{'distributed' functions cannot be 'static'}}
}
