// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -target %target-cpu-apple-ios13.1-macabi -typecheck -verify -I %S/Inputs/custom-modules %s

// REQUIRES: maccatalyst_support

import Foundation
import AvailabilityExtras

func test_unavailable_because_deprecated() {
  print(NSRealMemoryAvailable()) // expected-error {{APIs deprecated as of iOS 7 and earlier are unavailable in Swift}}
}

func test_swift_unavailable_wins() {
  unavailableWithOS() // expected-error {{'unavailableWithOS()' is unavailable in Swift}}
}

func test_maccatalyst_unavailable_wins() {
  availableOnIOSButUnavailableOnmacCatalyst() // expected-error {{'availableOnIOSButUnavailableOnmacCatalyst()' is unavailable}}
}

func test_maccatalyst_deprecated_wins() {
  availableOnIOSButDeprecatedOnmacCatalyst() // expected-warning {{'availableOnIOSButDeprecatedOnmacCatalyst()' was deprecated in Mac Catalyst 9.0}}
}


func test_ios_unavailable_is_also_unavailable_on_maccatalyst() {
  unavailableOnIOS() // expected-error {{'unavailableOnIOS()' is unavailable}}
}

func test_deprecation_on_ios_not_inherited_when_not_specified_on_maccatalyst() {
  deprecatedOniOSButNotOnmacCatalyst(); // no-warning
}

func test_ios_app_extension() {
  availableOnIOSButUnavailableOniOSAppExtension() // no-error
  availableOnIOSAppExtensionButUnavailableOnmacCatalystAppExtension() // no-error

  availableOnIOSButDeprecatedOniOSAppExtension() // no-warning
  availableOnIOSAppExtensionButDeprecatedOnmacCatalystAppExtension() // no-warning
}

// Test platform inheritance for imported decls unavailable in iOS.
// rdar://68597591

@available(iOS, unavailable)
func unavailableFunctionUsingAnUnavailableType(_ p: UnavailableOniOS) { }

@available(iOS, unavailable)
func unavailableOniOS(_ p: UnavailableOniOS) { } // ok

func functionUsingAnUnavailableType(_ p: UnavailableOniOS) { } // expected-error {{'UnavailableOniOS' is unavailable in Mac Catalyst}}

public extension UnavailableOniOS { // expected-error {{'UnavailableOniOS' is unavailable in Mac Catalyst}}
  func someMethod1(_ p: UnavailableOniOS) { } // expected-error {{'UnavailableOniOS' is unavailable in Mac Catalyst}}
}

@available(iOS, unavailable)
public extension UnavailableOniOS { // ok
  func someMethod2(_ p: UnavailableOniOS) { }
}

public extension AvailableOnMacCatalystOnly { } // ok

public extension UnavailableOnMacCatalystOnly { } // expected-error {{UnavailableOnMacCatalystOnly' is unavailable in Mac Catalyst}}

@available(iOS, unavailable)
@available(macCatalyst, introduced: 13.0)
struct StructAvailableOnMacCatalystOnly {
  func nestedCheck(_ p: AvailableOnMacCatalystOnly) {}
  func invertedNestedCheck(_ p: UnavailableOnMacCatalystOnly) {} // expected-error {{UnavailableOnMacCatalystOnly' is unavailable in Mac Catalyst}}
}

@available(iOS, introduced: 13.0)
@available(macCatalyst, unavailable)
struct StructUnavailableOnMacCatalystOnly {
  func nestedCheck(_ p: UnavailableOnMacCatalystOnly) {}
  func invertedNestedCheck(_ p: AvailableOnMacCatalystOnly) {} // Would error for an iOS target.
}
