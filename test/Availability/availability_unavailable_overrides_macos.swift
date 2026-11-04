// RUN: %target-typecheck-verify-swift -verify-additional-prefix swift6- -swift-version 6
// RUN: %if swift_future %{ %target-typecheck-verify-swift -verify-additional-prefix future- -swift-version future %}

// REQUIRES: OS=macosx

import Foundation

class Base {
  func macOSUnavailableOverride() {} // expected-note {{overridden declaration is here}}
  func macOSObsoletedOverride() {}
  @objc dynamic func unavailableExtensionOverride() {} // expected-note {{overridden declaration is here}}
  func iOSUnavailableOverride() {}
}

class Derived: Base {
  @available(macOS, unavailable)
  override func macOSUnavailableOverride() {}
  // expected-swift6-warning@-1 {{cannot override 'macOSUnavailableOverride' with a declaration that is marked unavailable; this will be an error in a future Swift language mode}}{{-1:1-+0:1=}}{{group-name=UnavailableOverride}}
  // expected-future-error@-2 {{cannot override 'macOSUnavailableOverride' with a declaration that is marked unavailable}}{{group-name=UnavailableOverride}}

  @available(macOS, obsoleted: 10.15) // OK, only hidden from source code.
  override func macOSObsoletedOverride() {}

  @available(iOS, unavailable) // OK, the override is available on macOS.
  override func iOSUnavailableOverride() {}
}

@available(*, unavailable)
extension Derived {
  @objc override func unavailableExtensionOverride() {}
  // expected-swift6-warning@-1 {{cannot override 'unavailableExtensionOverride' with a declaration that is marked unavailable; this will be an error in a future Swift language mode}}{{none}}
  // expected-future-error@-2 {{cannot override 'unavailableExtensionOverride' with a declaration that is marked unavailable}}{{none}}
}

// The overrides of an unavailable class are unreachable, so they don't need to
// be as available as the declarations they override.
@available(*, unavailable)
class UnavailableDerived: Base {
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
