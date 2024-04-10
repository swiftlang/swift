// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

actor SomeActor {}

distributed actor DA {}
// expected-error@-1{{distributed actor 'DA' requires explicit import of Distributed module}}

distributed actor class DAC {}
// expected-error@-1{{distributed' can only be applied to 'actor' definitions, and distributed actor-isolated async functions}}
// expected-error@-2{{keyword 'class' cannot be used as an identifier here}}

actor A {
  func normal() async {}
  distributed func dist() {} // expected-error{{'distributed' method can only be declared within 'distributed actor'}}
  distributed func distAsync() async {} // expected-error{{'distributed' method can only be declared within 'distributed actor'}}
}

actor B {
  distributed var neverOk: String { // expected-error{{distributed property 'neverOk' requires explicit import of Distributed module}}
    ""
  }
}

  // expected-error@+1{{distributed actor 'DA2' requires explicit import of Distributed module}}
distributed actor DA2 {

  func normal() async {}

  // expected-error@+1{{distributed instance method 'dist()' requires explicit import of Distributed module}}
  distributed func dist() {}

  // expected-error@+1{{distributed instance method 'distAsync()' requires explicit import of Distributed module}}
  distributed func distAsync() async {}

  // expected-error@+1{{distributed property 'neverOk' requires explicit import of Distributed module}}
  distributed var neverOk: String {
    ""
  }
}

