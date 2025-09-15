// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple
// REQUIRES: concurrency
// REQUIRES: distributed

actor SomeActor {}

distributed actor DA {}
// expected-error@-1{{distributed actor 'DA' declared without importing module 'Distributed'}}

distributed actor class DAC {}
// expected-error@-1{{distributed' can only be applied to 'actor' definitions, and distributed actor-isolated async functions}}
// expected-error@-2{{keyword 'class' cannot be used as an identifier here}}

actor A {
  func normal() async {}
  distributed func dist() {} // expected-error{{'distributed' method can only be declared within 'distributed actor'}}
  distributed func distAsync() async {} // expected-error{{'distributed' method can only be declared within 'distributed actor'}}
}

actor B {
  distributed var neverOk: String { // expected-error{{distributed property 'neverOk' declared without importing module 'Distributed'}}
    ""
  }
}

  // expected-error@+1{{distributed actor 'DA2' declared without importing module 'Distributed'}}
distributed actor DA2 {

  func normal() async {}

  // expected-error@+1{{distributed instance method 'dist()' declared without importing module 'Distributed'}}
  distributed func dist() {}

  // expected-error@+1{{distributed instance method 'distAsync()' declared without importing module 'Distributed'}}
  distributed func distAsync() async {}

  // expected-error@+1{{distributed property 'neverOk' declared without importing module 'Distributed'}}
  distributed var neverOk: String {
    ""
  }
}

