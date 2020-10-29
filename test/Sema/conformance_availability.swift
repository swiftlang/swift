// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-conformance-availability-errors

// REQUIRES: OS=macosx

public protocol Horse {}
func takesHorse<T : Horse>(_: T) {}

extension Horse {
  func giddyUp() {}
}

struct UsesHorse<T : Horse> {}

// Unconditional unavailability
public struct HasUnavailableConformance1 {}

@available(*, unavailable)
extension HasUnavailableConformance1 : Horse {}
// expected-note@-1 6{{conformance of 'HasUnavailableConformance1' to 'Horse' has been explicitly marked unavailable here}}

func passUnavailableConformance1(x: HasUnavailableConformance1) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance1' to 'Horse' is unavailable}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance1' to 'Horse' is unavailable}}
  _ = UsesHorse<HasUnavailableConformance1>.self // expected-error {{conformance of 'HasUnavailableConformance1' to 'Horse' is unavailable}}
}

@available(*, unavailable)
func passUnavailableConformance1a(x: HasUnavailableConformance1) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance1' to 'Horse' is unavailable}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance1' to 'Horse' is unavailable}}
  _ = UsesHorse<HasUnavailableConformance1>.self // expected-error {{conformance of 'HasUnavailableConformance1' to 'Horse' is unavailable}}
}

// Platform unavailability
public struct HasUnavailableConformance2 {}

@available(macOS, unavailable)
extension HasUnavailableConformance2 : Horse {}
// expected-note@-1 3{{conformance of 'HasUnavailableConformance2' to 'Horse' has been explicitly marked unavailable here}}

func passUnavailableConformance2(x: HasUnavailableConformance2) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance2' to 'Horse' is unavailable in macOS}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance2' to 'Horse' is unavailable in macOS}}
  _ = UsesHorse<HasUnavailableConformance2>.self // expected-error {{conformance of 'HasUnavailableConformance2' to 'Horse' is unavailable in macOS}}
}

@available(macOS, unavailable)
func passUnavailableConformance2a(x: HasUnavailableConformance2) {
  // This is allowed
  takesHorse(x)
  x.giddyUp()
}

// Swift version unavailability
public struct HasUnavailableConformance3 {}

@available(swift 12)
extension HasUnavailableConformance3 : Horse {}
// expected-note@-1 6{{conformance of 'HasUnavailableConformance3' to 'Horse' was introduced in Swift 12}}

func passUnavailableConformance3(x: HasUnavailableConformance3) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
  _ = UsesHorse<HasUnavailableConformance3>.self // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
}

@available(swift 12)
func passUnavailableConformance3a(x: HasUnavailableConformance3) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
  _ = UsesHorse<HasUnavailableConformance3>.self // expected-error {{conformance of 'HasUnavailableConformance3' to 'Horse' is unavailable}}
}

// Platform obsoleted
public struct HasUnavailableConformance4 {}

@available(macOS, obsoleted: 10.1)
extension HasUnavailableConformance4 : Horse {}
// expected-note@-1 6{{conformance of 'HasUnavailableConformance4' to 'Horse' was obsoleted in macOS 10.1}}

func passUnavailableConformance4(x: HasUnavailableConformance4) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable in macOS}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable in macOS}}
  _ = UsesHorse<HasUnavailableConformance4>.self // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable in macOS}}
}

@available(macOS, obsoleted: 10.1)
func passUnavailableConformance4a(x: HasUnavailableConformance4) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable in macOS}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable in macOS}}
  _ = UsesHorse<HasUnavailableConformance4>.self // expected-error {{conformance of 'HasUnavailableConformance4' to 'Horse' is unavailable in macOS}}
}

// Swift obsoleted
public struct HasUnavailableConformance5 {}

@available(swift, obsoleted: 4)
extension HasUnavailableConformance5 : Horse {}
// expected-note@-1 6{{conformance of 'HasUnavailableConformance5' to 'Horse' was obsoleted in Swift 4}}

func passUnavailableConformance5(x: HasUnavailableConformance5) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
  _ = UsesHorse<HasUnavailableConformance5>.self // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
}

@available(swift, obsoleted: 4)
func passUnavailableConformance5a(x: HasUnavailableConformance5) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
  _ = UsesHorse<HasUnavailableConformance5>.self // expected-error {{conformance of 'HasUnavailableConformance5' to 'Horse' is unavailable}}
}

// Unavailable with message
public struct HasUnavailableConformance6 {}

@available(*, unavailable, message: "This conformance is bad")
extension HasUnavailableConformance6 : Horse {}
// expected-note@-1 3{{conformance of 'HasUnavailableConformance6' to 'Horse' has been explicitly marked unavailable here}}

func passUnavailableConformance6(x: HasUnavailableConformance6) {
  takesHorse(x) // expected-error {{conformance of 'HasUnavailableConformance6' to 'Horse' is unavailable: This conformance is bad}}
  x.giddyUp() // expected-error {{conformance of 'HasUnavailableConformance6' to 'Horse' is unavailable: This conformance is bad}}
  _ = UsesHorse<HasUnavailableConformance6>.self // expected-error {{conformance of 'HasUnavailableConformance6' to 'Horse' is unavailable: This conformance is bad}}
}

// Deprecated
public struct HasDeprecatedConformance1 {}

@available(*, deprecated)
extension HasDeprecatedConformance1 : Horse {}

func passDeprecatedConformance1(x: HasDeprecatedConformance1) {
  takesHorse(x) // expected-warning {{conformance of 'HasDeprecatedConformance1' to 'Horse' is deprecated}}
  x.giddyUp() // expected-warning {{conformance of 'HasDeprecatedConformance1' to 'Horse' is deprecated}}
  _ = UsesHorse<HasDeprecatedConformance1>.self // expected-warning {{conformance of 'HasDeprecatedConformance1' to 'Horse' is deprecated}}
}

@available(*, deprecated)
func passDeprecatedConformance1a(x: HasDeprecatedConformance1) {
  takesHorse(x)
  x.giddyUp()
  _ = UsesHorse<HasDeprecatedConformance1>.self
}

// Deprecated with message
public struct HasDeprecatedConformance2 {}

@available(*, deprecated, message: "This conformance is deprecated")
extension HasDeprecatedConformance2 : Horse {}

func passDeprecatedConformance2(x: HasDeprecatedConformance2) {
  takesHorse(x) // expected-warning {{conformance of 'HasDeprecatedConformance2' to 'Horse' is deprecated: This conformance is deprecated}}
  x.giddyUp() // expected-warning {{conformance of 'HasDeprecatedConformance2' to 'Horse' is deprecated: This conformance is deprecated}}
  _ = UsesHorse<HasDeprecatedConformance2>.self // expected-warning {{conformance of 'HasDeprecatedConformance2' to 'Horse' is deprecated: This conformance is deprecated}}
}

@available(*, deprecated)
func passDeprecatedConformance2a(x: HasDeprecatedConformance2) {
  takesHorse(x)
  x.giddyUp()
  _ = UsesHorse<HasDeprecatedConformance2>.self
}

// Deprecated with version
public struct HasDeprecatedConformance3 {}

@available(macOS, introduced: 10.7, deprecated: 10.8)
extension HasDeprecatedConformance3 : Horse {}

func passDeprecatedConformance3(x: HasDeprecatedConformance3) {
  takesHorse(x) // expected-warning {{conformance of 'HasDeprecatedConformance3' to 'Horse' was deprecated in macOS 10.8}}
  x.giddyUp() // expected-warning {{conformance of 'HasDeprecatedConformance3' to 'Horse' was deprecated in macOS 10.8}}
  _ = UsesHorse<HasDeprecatedConformance3>.self // expected-warning {{conformance of 'HasDeprecatedConformance3' to 'Horse' was deprecated in macOS 10.8}}
}

func passDeprecatedConformance3a(x: HasDeprecatedConformance3) {
  if #available(macOS 10.8, *) {
  } else {
    // This branch is dead with our minimum deployment target, so don't emit
    // deprecation diagnostics in it.
    takesHorse(x)
    x.giddyUp()
    _ = UsesHorse<HasDeprecatedConformance3>.self
  }
}

// Availability with version
public struct HasAvailableConformance1 {}

@available(macOS 100, *)
extension HasAvailableConformance1 : Horse {}

// These availability violations are errors because this test passes the
// -enable-conformance-availability-errors flag. See the other test case
// in test/Sema/conformance_availability_warn.swift for the same example
// but without this flag.

func passAvailableConformance1(x: HasAvailableConformance1) { // expected-note 3{{add @available attribute to enclosing global function}}
  takesHorse(x) // expected-error {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}

  x.giddyUp() // expected-error {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}

  _ = UsesHorse<HasAvailableConformance1>.self // expected-error {{conformance of 'HasAvailableConformance1' to 'Horse' is only available in macOS 100 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
}

@available(macOS 100, *)
func passAvailableConformance1a(x: HasAvailableConformance1) {
  takesHorse(x)
  x.giddyUp()
  _ = UsesHorse<HasAvailableConformance1>.self
}