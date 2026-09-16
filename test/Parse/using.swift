// RUN: %target-typecheck-verify-swift -enable-experimental-feature DefaultIsolationPerFile

// REQUIRES: swift_feature_DefaultIsolationPerFile

// REQUIRES: concurrency

using @MainActor
// expected-note@-1:1 {{file-level default isolation previously declared here}}

using nonisolated
// expected-error@-1:1 {{invalid redeclaration of file-level default isolation}}

using @Test // expected-error {{cannot find type 'Test' in scope}}
// expected-note@-1:7 {{'using' supports '@MainActor', 'nonisolated', '@available', and '@diagnose'}}
using test // expected-error@:7 {{expected '@MainActor', 'nonisolated', '@available', or '@diagnose' after 'using'}}

using @inlinable
// expected-error@-1 {{'@inlinable' is not valid in a 'using' declaration}}
// expected-note@-2 {{'using' supports '@MainActor', 'nonisolated', '@available', and '@diagnose'}}

using @backDeployed(before: macOS 13.0)
// expected-error@-1:8 {{'@backDeployed' is not valid in a 'using' declaration}}
// expected-note@-2:8 {{'using' supports '@MainActor', 'nonisolated', '@available', and '@diagnose'}}

using @backDeployed(before: macOS 13.0, iOS 16.0)
// expected-error@-1:8 {{'@backDeployed' is not valid in a 'using' declaration}}
// expected-note@-2:8 {{'using' supports '@MainActor', 'nonisolated', '@available', and '@diagnose'}}

using @_originallyDefinedIn(module: "Other", macOS 13.0)
// expected-error@-1:8 {{'@_originallyDefinedIn' is not valid in a 'using' declaration}}
// expected-note@-2:8 {{'using' supports '@MainActor', 'nonisolated', '@available', and '@diagnose'}}

using @_originallyDefinedIn(module: "Other", macOS 13.0, iOS 16.0)
// expected-error@-1:8 {{'@_originallyDefinedIn' is not valid in a 'using' declaration}}
// expected-note@-2:8 {{'using' supports '@MainActor', 'nonisolated', '@available', and '@diagnose'}}

do {
  using // expected-warning {{expression of type 'Int' is unused}}
  @MainActor
// expected-error@+1 {{expected declaration}}
}

using @diagnose(StrictMemorySafety, as: error)

// We have a tailored diagnostic for global actors that aren't MainActor.
@globalActor
actor MyActor { // expected-note@:7 {{'MyActor' declared here}}
  static let shared = MyActor()
  using @MyActor
  // expected-error@-1:3 {{declaration is only valid at file scope}}
  // TODO: we don't diagnose nested 'using' misuse since the request doesn't see it.
}

using @MyActor // expected-error@:7 {{global actor 'MyActor' is not valid in a 'using' declaration}}
// expected-note@-1:7 {{file-level default isolation must be '@MainActor' or 'nonisolated'}}

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
  using @MainActor // expected-error@:3 {{declaration is only valid at file scope}}
}

func test() {
  using @MainActor // expected-error@:3 {{declaration is only valid at file scope}}
}

struct S {
  var x: Int {
    using @MainActor // expected-error@:5 {{declaration is only valid at file scope}}
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
  // expected-error@-1:9 {{declaration is only valid at file scope}}
  // expected-error@-2:4 {{attribute cannot be attached to a 'using' declaration}}
}
