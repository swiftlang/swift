// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -target %target-cpu-apple-ios13.1-macabi -application-extension -typecheck -verify -I %S/Inputs/custom-modules %s

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
  availableOnIOSButUnavailableOnmacCatalyst() // expected-error {{'availableOnIOSButUnavailableOnmacCatalyst()' is unavailable in Mac Catalyst}}
}

func test_maccatalyst_deprecated_wins() {
  availableOnIOSButDeprecatedOnmacCatalyst() // expected-warning {{'availableOnIOSButDeprecatedOnmacCatalyst()' was deprecated in Mac Catalyst 9.0}}
}

func test_ios_unavailable_is_also_unavailable_on_maccatalyst() {
  unavailableOnIOS() // expected-error {{'unavailableOnIOS()' is unavailable in Mac Catalyst}}
}

func test_deprecation_on_ios_not_inherited_when_not_specified_on_maccatalyst() {
  deprecatedOniOSButNotOnmacCatalyst(); // no-warning
}

func test_ios_app_extension() {
  availableOnIOSButUnavailableOniOSAppExtension() // expected-error {{'availableOnIOSButUnavailableOniOSAppExtension()' is unavailable in application extensions for Mac Catalyst}}
  availableOnIOSAppExtensionButUnavailableOnmacCatalystAppExtension() // expected-error {{'availableOnIOSAppExtensionButUnavailableOnmacCatalystAppExtension()' is unavailable in application extensions for Mac Catalyst}}

  availableOnIOSButDeprecatedOniOSAppExtension() // expected-warning {{'availableOnIOSButDeprecatedOniOSAppExtension()' was deprecated in application extensions for Mac Catalyst 13.1}}
  availableOnIOSAppExtensionButDeprecatedOnmacCatalystAppExtension() // expected-warning {{'availableOnIOSAppExtensionButDeprecatedOnmacCatalystAppExtension()' was deprecated in application extensions for Mac Catalyst 9.0}}
}
