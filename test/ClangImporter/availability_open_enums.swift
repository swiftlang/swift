// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules -swift-version 4 %s

// REQUIRES: objc_interop

import Foundation
import AvailabilityExtras

func exhaustiveSwitch(e: NSEnumAddedCasesIn2017) {
  switch e { // expected-warning{{switch must be exhaustive}}
    // expected-note@-1{{do you want to add a default clause?}}
  case .existingCaseOne:
    return
  case .existingCaseTwo:
    return
  case .existingCaseThree:
    return
  }
}