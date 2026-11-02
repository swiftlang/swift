// RUN: %target-typecheck-verify-swift -verify-additional-prefix swift6- -swift-version 6
// RUN: %if swift7 %{ %target-typecheck-verify-swift -verify-additional-prefix swift7- -swift-version 7 %}

// REQUIRES: OS=macosx

import Foundation

class Base {
  func universallyUnavailableOverride() {} // expected-note {{overridden declaration is here}}
  func macOSUnavailableOverride() {} // expected-note {{overridden declaration is here}}
  func swiftIntroducedOverride() {}
  func swiftObsoletedOverride() {}
  func macOSObsoletedOverride() {}
  @objc dynamic func unavailableExtensionOverride() {} // expected-note {{overridden declaration is here}}
  func iOSUnavailableOverride() {}
  func deprecatedOverride() {}
  var storedProperty = 0 // expected-note {{overridden declaration is here}}
}

class Derived: Base {
  @available(*, unavailable)
  override func universallyUnavailableOverride() {}
  // expected-swift6-warning@-1 {{cannot override 'universallyUnavailableOverride' with a declaration that is marked unavailable; this will be an error in a future Swift language mode}}{{21:1-22:1=}}
  // expected-swift7-error@-2 {{cannot override 'universallyUnavailableOverride' with a declaration that is marked unavailable}}

  @available(macOS, unavailable)
  override func macOSUnavailableOverride() {}
  // expected-swift6-warning@-1 {{cannot override 'macOSUnavailableOverride' with a declaration that is marked unavailable; this will be an error in a future Swift language mode}}{{group-name=UnavailableOverride}}
  // expected-swift7-error@-2 {{cannot override 'macOSUnavailableOverride' with a declaration that is marked unavailable}}{{group-name=UnavailableOverride}}

  @available(swift, introduced: 99) // OK, only hidden from source code.
  override func swiftIntroducedOverride() {}

  @available(swift, obsoleted: 1) // OK, only hidden from source code.
  override func swiftObsoletedOverride() {}

  @available(macOS, obsoleted: 10.15) // OK, only hidden from source code.
  override func macOSObsoletedOverride() {}

  @available(iOS, unavailable) // OK, the override is available on macOS.
  override func iOSUnavailableOverride() {}

  @available(*, deprecated) // OK, deprecation does not affect availability.
  override func deprecatedOverride() {}

  override var storedProperty: Int {
    get { 1 }
    @available(*, unavailable)
    set {}
    // expected-swift6-warning@-1 {{cannot override setter for 'storedProperty' with a declaration that is marked unavailable; this will be an error in a future Swift language mode}}
    // expected-swift7-error@-2 {{cannot override setter for 'storedProperty' with a declaration that is marked unavailable}}
  }
}

@available(*, unavailable)
extension Derived {
  @objc override func unavailableExtensionOverride() {}
  // expected-swift6-warning@-1 {{cannot override 'unavailableExtensionOverride' with a declaration that is marked unavailable; this will be an error in a future Swift language mode}}{{none}}
  // expected-swift7-error@-2 {{cannot override 'unavailableExtensionOverride' with a declaration that is marked unavailable}}{{none}}
}

// The overrides of an unavailable class are unreachable, so they don't need to
// be as available as the declarations they override.
@available(*, unavailable)
class UnavailableDerived: Base {
  @available(*, unavailable)
  override func universallyUnavailableOverride() {}

  @available(macOS, unavailable)
  override func macOSUnavailableOverride() {}
}

// A requirement restated in a refining protocol has no implementation of its
// own, and calls to the requirement dispatch to the witness of the conforming
// type.
@objc protocol BaseProto {
  func requirement()
  func macOSRequirement()
}

@objc protocol DerivedProto: BaseProto {
  @available(*, unavailable)
  func requirement()

  @available(macOS, unavailable)
  func macOSRequirement()
}
