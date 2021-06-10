// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// ^^^^ notice the, on purpose, missing '-enable-experimental-distributed'
// REQUIRES: concurrency

actor SomeActor {}

@available(SwiftStdlib 5.5, *)
distributed actor DA {} // expected-error{{'_Distributed' module not imported, required for 'distributed actor'}}
// expected-error@-1{{class 'DA' has no initializers}}

@available(SwiftStdlib 5.5, *)
distributed actor class DAC {} // expected-error{{distributed' can only be applied to 'actor' definitions, and distributed actor-isolated async functions}}
// expected-error@-1{{keyword 'class' cannot be used as an identifier here}}

actor A {
  func normal() async {}
  distributed func dist() {} // expected-error{{'distributed' function can only be declared within 'distributed actor'}}
  distributed func distAsync() async {} // expected-error{{'distributed' function can only be declared within 'distributed actor'}}

  distributed var neverOk: String { // expected-error{{'distributed' modifier cannot be applied to this declaration}}
    "vars are not allowed to be distributed *ever* anyway"
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor DA2 { // expected-error{{'_Distributed' module not imported, required for 'distributed actor'}}
  // expected-error@-1{{class 'DA2' has no initializers}}
  func normal() async {}
  distributed func dist() {}
  distributed func distAsync() async {}

  distributed var neverOk: String { // expected-error{{'distributed' modifier cannot be applied to this declaration}}
    "vars are not allowed to be distributed *ever* anyway"
  }
}

