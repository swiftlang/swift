// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// ^^^^ notice the, on purpose, missing '-enable-experimental-distributed'
// REQUIRES: concurrency

actor SomeActor {}

distributed actor DA {} // expected-error{{'_Distributed' module not imported, required for 'distributed actor'}}
// expected-error@-1{{class 'DA' has no initializers}}
distributed actor class DAC {} // expected-error{{'_Distributed' module not imported, required for 'distributed actor'}}
// expected-error@-1{{class 'DAC' has no initializers}}
// expected-warning@-2{{'actor class' has been renamed to 'actor'}}

actor A {
  func normal() async {}
  distributed func dist() {} // expected-error{{'distributed' function can only be declared within 'distributed actor'}}
  distributed func distAsync() async {} // expected-error{{'distributed' function can only be declared within 'distributed actor'}}

  distributed var neverOk: String { // expected-error{{'distributed' modifier cannot be applied to this declaration}}
    "vars are not allowed to be distributed *ever* anyway"
  }
}

distributed actor DA2 { // expected-error{{'_Distributed' module not imported, required for 'distributed actor'}}
  // expected-error@-1{{class 'DA2' has no initializers}}
  func normal() async {}
  distributed func dist() {}
  distributed func distAsync() async {}

  distributed var neverOk: String { // expected-error{{'distributed' modifier cannot be applied to this declaration}}
    "vars are not allowed to be distributed *ever* anyway"
  }
}

extension DA2 {
  static func _remote_dist(actor: DA2) async throws {}
  static func _remote_distAsync(actor: DA2) async throws {}
}
