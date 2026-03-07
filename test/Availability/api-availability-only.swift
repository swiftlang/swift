/// Verify that -check-api-availability-only has no effect since it is deprecated.
// RUN: %target-typecheck-verify-swift -check-api-availability-only -verify

// REQUIRES: OS=macosx

@available(macOS 52, *)
public struct S {}

@available(macOS 51, *)
public func newFunc() {
// expected-note@-1 {{update '@available' attribute on enclosing}}
  _ = S() // expected-error {{'S' is only available in}}
  // expected-note @-1 {{add 'if #available' version check}}
}
