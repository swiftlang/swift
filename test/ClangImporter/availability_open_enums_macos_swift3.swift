// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules -swift-version 3 -target x86_64-apple-macosx10.13 %s 

// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import Foundation
import AvailabilityExtras

func exhaustiveSwitch(e: NSEnumAddedCasesIn2017) {
  switch e { // expected-warning{{switch must be exhaustive}}
    // expected-note@-1{{add missing case: '.newCaseOne'}}
  case .existingCaseOne:
    return
  case .existingCaseTwo:
    return
  case .existingCaseThree:
    return    
  }
}