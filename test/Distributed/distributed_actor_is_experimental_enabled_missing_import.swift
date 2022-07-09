// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

actor SomeActor {}

distributed actor DA {}
// expected-error@-1{{'Distributed' module not imported, required for 'distributed actor'}}

distributed actor class DAC {}
// expected-error@-1{{distributed' can only be applied to 'actor' definitions, and distributed actor-isolated async functions}}
// expected-error@-2{{keyword 'class' cannot be used as an identifier here}}

actor A {
  func normal() async {}
  distributed func dist() {} // expected-error{{'distributed' method can only be declared within 'distributed actor'}}
  distributed func distAsync() async {} // expected-error{{'distributed' method can only be declared within 'distributed actor'}}
}

actor B {
  distributed var neverOk: String { // expected-error{{'Distributed' module not imported, required for 'distributed actor'}}
    ""
  }
}

distributed actor DA2 {
  // expected-error@-1{{'Distributed' module not imported, required for 'distributed actor'}}
  func normal() async {}
  distributed func dist() {}
  distributed func distAsync() async {}

  distributed var neverOk: String { // expected-error{{'Distributed' module not imported, required for 'distributed actor'}}
    ""
  }
}

