// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: concurrency
// REQUIRES: distributed

actor SomeActor {}

distributed actor DA {}
// expected-error@-1{{'Distributed' module not imported, required for 'distributed actor'}}

distributed actor DA2 {
  // expected-error@-1{{'Distributed' module not imported, required for 'distributed actor'}}
  func normal() async {}
  distributed func dist() {}
  distributed func distAsync() async {}

  distributed var neverOk: String { // expected-error{{'Distributed' module not imported, required for 'distributed actor'}}
    ""
  }
}

extension DA2 {
  distributed func ohNo() {}
}
