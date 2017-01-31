// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules %s

// REQUIRES: OS=watchos

import Foundation
import AvailabilityExtras

func test_unavailable_because_deprecated() {
  // This is transcribed by clang to be deprecated on watchOS 2.0
  // We don't currently treat these unavailable, but the policy decision
  // hasn't been made yet.
  // rdar://problem/20948019 tracks changing this, if needed, when the
  // policy decision is made. 
  print(NSRealMemoryAvailable()) // expected-warning {{'NSRealMemoryAvailable()' was deprecated in watchOS 2.0}}
}

func test_swift_unavailable_wins() {
  unavailableWithOS() // expected-error {{'unavailableWithOS()' is unavailable in Swift}}
}
