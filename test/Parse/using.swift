// RUN: %target-typecheck-verify-swift -enable-experimental-feature DefaultIsolationPerFile

// REQUIRES: swift_feature_DefaultIsolationPerFile

// REQUIRES: concurrency

using @MainActor
// expected-note@-1 {{default isolation was previously declared here}}

using nonisolated
// expected-error@-1 {{invalid redeclaration of file-level default actor isolation}}

using @Test // expected-error {{default isolation can only be set to '@MainActor' or 'nonisolated'}}
using test // expected-error {{default isolation can only be set to '@MainActor' or 'nonisolated'}}

do {
  using // expected-warning {{expression of type 'Int' is unused}}
  @MainActor
// expected-error@+1 {{expected declaration}}
}

do {
  using // expected-warning {{expression of type 'Int' is unused}}
  nonisolated // expected-error {{cannot find 'nonisolated' in scope}}
}

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
