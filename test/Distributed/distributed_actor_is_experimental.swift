// RUN: %target-typecheck-verify-swift  -disable-availability-checking
// ^^^^ notice the, on purpose, missing '-enable-experimental-distributed'
// REQUIRES: concurrency
// REQUIRES: distributed

actor SomeActor {}

@available(SwiftStdlib 5.5, *)
distributed actor DA {}
// expected-error@-1{{'distributed' modifier is only valid when experimental distributed support is enabled}}

@available(SwiftStdlib 5.5, *)
distributed actor class DAC {}
// expected-error@-1{{'distributed' modifier is only valid when experimental distributed support is enabled}}
// expected-error@-2{{keyword 'class' cannot be used as an identifier here}}

actor A {
  func normal() async {}
  distributed func dist() {}
  // expected-error@-1{{'distributed' modifier is only valid when experimental distributed support is enabled}}
  distributed func distAsync() async {}
  // expected-error@-1{{'distributed' modifier is only valid when experimental distributed support is enabled}}

  distributed var neverOk: String {
    // expected-error@-1{{'distributed' modifier is only valid when experimental distributed support is enabled}}
    "vars are not allowed to be distributed *ever* anyway"
  }
}

@available(SwiftStdlib 5.5, *)
distributed actor DA2 {
  // expected-error@-1{{'distributed' modifier is only valid when experimental distributed support is enabled}}
  func normal() async {}
  distributed func dist() {}
  // expected-error@-1{{'distributed' modifier is only valid when experimental distributed support is enabled}}
  distributed func distAsync() async {}
  // expected-error@-1{{'distributed' modifier is only valid when experimental distributed support is enabled}}

  distributed var neverOk: String {
    // expected-error@-1{{'distributed' modifier is only valid when experimental distributed support is enabled}}
    "vars are not allowed to be distributed *ever* anyway"
  }
}

