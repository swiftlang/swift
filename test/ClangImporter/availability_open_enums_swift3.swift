// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules -swift-version 3 -enable-nonfrozen-enum-exhaustivity-diagnostics %s

// REQUIRES: objc_interop

import Foundation
import AvailabilityExtras

func exhaustiveSwitch(e: NSEnumAddedCasesIn2017) {
  switch e { // expected-warning{{switch must be exhaustive}}
    // expected-note@-1{{add missing case: '.newCaseOne'}}
    // expected-note@-2{{handle unknown values using "@unknown default"}}
  case .existingCaseOne:
    return
  case .existingCaseTwo:
    return
  case .existingCaseThree:
    return    
  }
}