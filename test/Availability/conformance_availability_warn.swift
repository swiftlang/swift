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

// These availability violations are warnings because this test does not pass
// -swift-version 6.
// See the other test case in test/Sema/conformance_availability.swift for the
// same example but with -swift-version 6.

func passAvailableConformance1(x: HasAvailableConformance1) { // expected-note 6{{add '@available' attribute to enclosing global function}}
  takesHorse(x) // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer; this is an error in the Swift 6 language mode}}
  // expected-note@-1 {{add 'if #available' version check}}

  takesHorseExistential(x) // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer; this is an error in the Swift 6 language mode}}
  // expected-note@-1 {{add 'if #available' version check}}
  
  x.giddyUp() // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer; this is an error in the Swift 6 language mode}}
  // expected-note@-1 {{add 'if #available' version check}}
  
  _ = x.isGalloping // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer; this is an error in the Swift 6 language mode}}
  // expected-note@-1 {{add 'if #available' version check}}
  
  _ = x[keyPath: \.isGalloping] // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer; this is an error in the Swift 6 language mode}}
  // expected-note@-1 {{add 'if #available' version check}}

  _ = UsesHorse<HasAvailableConformance1>.self // expected-warning {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer; this is an error in the Swift 6 language mode}}
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

// Explicit unavailability
public struct HasAvailableConformance2 {}

@available(*, unavailable)
extension HasAvailableConformance2 : Horse {} // expected-note 6 {{conformance of 'HasAvailableConformance2' to 'Horse' has been explicitly marked unavailable here}}

// Some availability diagnostics become warnings in Swift 5 mode without
// because they were incorrectly accepted before and rejecting them would break
// source compatibility. Others are unaffected because they have always been
// rejected.

func passAvailableConformance2(x: HasAvailableConformance2) {
  takesHorse(x) // expected-error {{conformance of 'HasAvailableConformance2' to 'Horse' is unavailable}}
  takesHorseExistential(x) // expected-warning {{conformance of 'HasAvailableConformance2' to 'Horse' is unavailable; this is an error in the Swift 6 language mode}}
  x.giddyUp() // expected-error {{conformance of 'HasAvailableConformance2' to 'Horse' is unavailable}}
  _ = x.isGalloping // expected-error {{conformance of 'HasAvailableConformance2' to 'Horse' is unavailable}}
  _ = x[keyPath: \.isGalloping] // expected-error {{conformance of 'HasAvailableConformance2' to 'Horse' is unavailable}}
  _ = UsesHorse<HasAvailableConformance2>.self // expected-error {{conformance of 'HasAvailableConformance2' to 'Horse' is unavailable}}
}

@available(*, unavailable)
func passAvailableConformance2a(x: HasAvailableConformance2) {
  takesHorse(x)
  takesHorseExistential(x)
  x.giddyUp()
  _ = x.isGalloping
  _ = UsesHorse<HasAvailableConformance2>.self
}

