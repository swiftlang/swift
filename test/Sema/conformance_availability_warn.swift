// RUN: %target-typecheck-verify-swift -swift-version 5

// REQUIRES: OS=macosx

public protocol Horse {}
func takesHorse<T : Horse>(_: T) {}

extension Horse {
  func giddyUp() {}
}

struct UsesHorse<T : Horse> {}

// Availability with version
public struct HasAvailableConformance1 {}

@available(macOS 100, *)
extension HasAvailableConformance1 : Horse {}

// These availability violations are warnings because this test does not
// pass the -enable-conformance-availability-errors flag. See the other
// test case in test/Sema/conformance_availability.swift for the same
// example but with this flag.

func passAvailableConformance1(x: HasAvailableConformance1) { // expected-note 3{{add @available attribute to enclosing global function}}
  takesHorse(x) // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}

  x.giddyUp() // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}

  _ = UsesHorse<HasAvailableConformance1>.self // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
}

@available(macOS 100, *)
func passAvailableConformance1a(x: HasAvailableConformance1) {
  takesHorse(x)
  x.giddyUp()
  _ = UsesHorse<HasAvailableConformance1>.self
}