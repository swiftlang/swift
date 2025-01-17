// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s -require-explicit-availability=warn
// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s -library-level=api

// This test should pass for all Apple platforms.
// REQUIRES: VENDOR=apple

public struct NoAvailabilityStruct { // expected-warning {{public declarations should have an availability attribute with an introduction version}}
  public func method() { }
}

@available(*, unavailable)
public struct UnavailableStruct {
  public func okMethod() { }
}

public func noAvailabilityFunc() { } // expected-warning {{public declarations should have an availability attribute with an introduction version}}

@available(macOS, introduced: 10.10)
@available(iOS, introduced: 8)
@available(watchOS, introduced: 2)
@available(tvOS, introduced: 9)
@available(visionOS, introduced: 1)
public func hasAvailability() { }
