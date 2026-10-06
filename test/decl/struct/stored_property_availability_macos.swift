// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=macosx

struct HasStoredProperties {
  @available(macOS, unavailable) // expected-error {{stored properties cannot be marked unavailable with '@available'}}
  var unavailableStored: Int

  @available(*, unavailable) // expected-error {{stored properties cannot be marked unavailable with '@available'}}
  var neverAvailableStored: Int

  @available(macOS 50, *) // expected-error {{stored properties cannot be marked potentially unavailable with '@available'}}
  var potentiallyUnavailableStored: Int

  // Computed properties may be unavailable or potentially unavailable.
  @available(macOS, unavailable)
  var unavailableComputed: Int { return 0 }

  @available(macOS 50, *)
  var potentiallyUnavailableComputed: Int { return 0 }

  // Static stored properties are lazily initialized, so they may be
  // unavailable or potentially unavailable.
  @available(macOS, unavailable)
  static var unavailableStatic: Int = 0

  @available(macOS 50, *)
  static var potentiallyUnavailableStatic: Int = 0
}

// Stored properties of a type that is itself unavailable may be marked
// unavailable.
@available(macOS, unavailable)
struct UnavailableStruct {
  @available(macOS, unavailable)
  var stored: Int
}
