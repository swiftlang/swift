// RUN: %target-typecheck-verify-swift -swift-version 6

@ MainActor  // expected-error {{extraneous whitespace between '@' and attribute name}}
class Foo {
  func foo(_ x: @ escaping () -> Int) {} // expected-error {{extraneous whitespace between '@' and attribute name}}
}
