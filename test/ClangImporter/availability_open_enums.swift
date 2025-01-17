// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules -enable-nonfrozen-enum-exhaustivity-diagnostics %s

// REQUIRES: objc_interop
// UNSUPPORTED: OS=xros

import Foundation
import AvailabilityExtras

func exhaustiveSwitch(e: NSEnumAddedCasesIn2017) {
  switch e { // expected-error{{switch must be exhaustive}}
    // expected-note@-1{{add missing case: '.newCaseOne'}}
    // expected-note@-2{{handle unknown values using "@unknown default"}}
    // expected-note@-3 {{add missing cases}}
  case .existingCaseOne:
    return
  case .existingCaseTwo:
    return
  case .existingCaseThree:
    return    
  }
}
