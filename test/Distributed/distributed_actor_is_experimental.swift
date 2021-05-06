// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// ^^^^ notice the, on purpose, missing '-enable-experimental-distributed'
// REQUIRES: concurrency

actor SomeActor {}

distributed actor DA {} // expected-error{{'distributed' modifier is only valid when experimental distributed is enabled. Enable it with -enable-experimental-distributed}}
distributed actor class DAC {} // expected-error{{'distributed' modifier is only valid when experimental distributed is enabled. Enable it with -enable-experimental-distributed}}
// expected-warning@-1{{'actor class' has been renamed to 'actor'}}

actor A {
  func normal() async {}
  distributed func dist() {} // expected-error{{'distributed' modifier is only valid when experimental distributed is enabled. Enable it with -enable-experimental-distributed}}
  distributed func distAsync() async {} // expected-error{{'distributed' modifier is only valid when experimental distributed is enabled. Enable it with -enable-experimental-distributed}}

  distributed var neverOk: String { // expected-error{{'distributed' modifier is only valid when experimental distributed is enabled. Enable it with -enable-experimental-distributed}}
    "vars are not allowed to be distributed *ever* anyway"
  }
}
