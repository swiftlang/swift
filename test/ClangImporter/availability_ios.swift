// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -verify-ignore-unrelated -I %S/Inputs/custom-modules %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -verify-ignore-unrelated -I %S/Inputs/custom-modules -application-extension %s -verify-additional-prefix extension-
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -verify-ignore-unrelated -I %S/Inputs/custom-modules -application-extension-library %s -verify-additional-prefix extension-

// REQUIRES: OS=ios
// UNSUPPORTED: OS=maccatalyst

import Foundation
import AvailabilityExtras

func test_unavailable_because_deprecated() {
  print(NSRealMemoryAvailable()) // expected-error {{APIs deprecated as of iOS 7 and earlier are unavailable in Swift}}
}

func test_swift_unavailable_wins() {
  unavailableWithOS() // expected-error {{'unavailableWithOS()' is unavailable in Swift}}
}


@available(iOS, introduced: 1.0)
@available(macCatalyst, introduced: 1.0, obsoleted: 2.0)
func obsoletedOnMacCatalystButNotIOS() { }

obsoletedOnMacCatalystButNotIOS() // no-error

@available(iOS, introduced: 1.0)
@available(macCatalyst, introduced: 1.0, deprecated: 2.0)
func deprecatedOnMacCatalystButNotIOS() { }

@available(iOS, introduced: 8.0)
func maccatalyst_tests() {
  deprecatedOnMacCatalystButNotIOS() // no-warning

  availableOnIOSButUnavailableOniOSAppExtension() // expected-extension-error {{'availableOnIOSButUnavailableOniOSAppExtension()' is unavailable in application extensions for iOS}}
  availableOnIOSAppExtensionButUnavailableOnmacCatalystAppExtension() // no-error

  availableOnIOSButDeprecatedOniOSAppExtension() // expected-extension-warning {{'availableOnIOSButDeprecatedOniOSAppExtension()' was deprecated in application extensions for iOS 9.0}}
  availableOnIOSAppExtensionButDeprecatedOnmacCatalystAppExtension() // no-warning
}
