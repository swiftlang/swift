// RUN: %target-typecheck-verify-swift -swift-version 5

// REQUIRES: OS=macosx

public protocol Horse {}
func takesHorse<T : Horse>(_: T) {}
func takesHorseExistential(_: Horse) {}

extension Horse {
  func giddyUp() {}
  var isGalloping: Bool { true }
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

func passAvailableConformance1(x: HasAvailableConformance1) { // expected-note 6{{add @available attribute to enclosing global function}}
  takesHorse(x) // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}

  takesHorseExistential(x) // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
  
  x.giddyUp() // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
  
  _ = x.isGalloping // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
  
  _ = x[keyPath: \.isGalloping] // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}

  _ = UsesHorse<HasAvailableConformance1>.self // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
}

@available(macOS 100, *)
func passAvailableConformance1a(x: HasAvailableConformance1) {
  takesHorse(x)
  takesHorseExistential(x)
  x.giddyUp()
  _ = x.isGalloping
  _ = UsesHorse<HasAvailableConformance1>.self
}
