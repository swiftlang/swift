/// Inlinable functions should check availability by ignoring the current
/// deployment target as clients could inline the function in a lower target.

// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx11.0
// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.10

// REQUIRES: OS=macosx

@available(macOS 10.10, *)
@inlinable
public func availMacOS10() {
  availMacOS11() // expected-error {{'availMacOS11()' is only available in macOS 11.0 or newer}}
  // expected-note @-1 {{add 'if #available' version check}}

  if #available(macOS 11.0, *) {
    availMacOS11()
  } else {
    availMacOS11() // expected-error {{'availMacOS11()' is only available in macOS 11.0 or newer}}
    // expected-note @-1 {{add 'if #available' version check}}
  }

  if #available(macOS 10.15, *) {
    availMacOS11() // expected-error {{'availMacOS11()' is only available in macOS 11.0 or newer}}
    // expected-note @-1 {{add 'if #available' version check}}
  }

  func nestedFunc() {
    availMacOS11() // expected-error {{'availMacOS11()' is only available in macOS 11.0 or newer}}
    // expected-note @-1 {{add 'if #available' version check}}
  }
}

@available(macOS 11.0, *)
public func availMacOS11() { }

@available(macOS 10.10, *)
public struct StructAvailMacOS10 {
  @inlinable
  public func availabilityFromTheContextInlinable() {
  // expected-note @-1 {{add @available attribute to enclosing instance method}}
    availMacOS11() // expected-error {{'availMacOS11()' is only available in macOS 11.0 or newer}}
    // expected-note @-1 {{add 'if #available' version check}}

    availabilityFromContext()
  }

  public func availabilityFromContext() {}
}
