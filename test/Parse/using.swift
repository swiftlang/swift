// RUN: %target-typecheck-verify-swift -enable-experimental-feature DefaultIsolationPerFile

// REQUIRES: swift_feature_DefaultIsolationPerFile

// REQUIRES: concurrency

using @MainActor
using nonisolated

using @Test // expected-error {{'using' declaration does not support 'Test' attribute}}
using test // expected-error {{'using' declaration does not support 'test' modifier}}

using
@MainActor

using
nonisolated

do {
  func
  using (x: Int) {}

  using(x: 42)
}

do {
  func
  using
  (x: Int) {}

  using(x: 42)
}

let
  using = 42

let (x: Int, using: String) = (x: 42, using: "")

do {
  using @MainActor // expected-error {{declaration is only valid at file scope}}
}

func test() {
  using @MainActor // expected-error {{declaration is only valid at file scope}}
}

struct S {
  var x: Int {
    using @MainActor // expected-error {{declaration is only valid at file scope}}
  }

  using @MainActor func test() {}
  // expected-error@-1 {{declaration is only valid at file scope}}
  // expected-error@-2 {{consecutive declarations on a line must be separated by ';'}}

  using nonisolated subscript(a: Int) -> Bool { false }
  // expected-error@-1 {{declaration is only valid at file scope}}
  // expected-error@-2 {{consecutive declarations on a line must be separated by ';'}}
}

do {
  @objc using @MainActor
  // expected-error@-1 {{expected expression}}
  // expected-error@-2 {{declaration is only valid at file scope}}
}
