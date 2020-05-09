// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules -application-extension %s

// REQUIRES: OS=macosx

import Foundation
import AvailabilityExtras

func test_unavailable_because_deprecated() {
  _ = NSRealMemoryAvailable() // expected-error {{APIs deprecated as of macOS 10.9 and earlier are unavailable in Swift}}
}

func test_swift_unavailable_wins() {
  unavailableWithOS() // expected-error {{'unavailableWithOS()' is unavailable in Swift}}
}
