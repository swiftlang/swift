// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-ide-test -print-module -module-to-print Availability -I %t -source-filename=%t/main.swift > %t/Availability.txt
// RUN: %FileCheck %s --check-prefix=CHECK < %t/Availability.txt

// REQUIRES: OS=macosx

//--- module.modulemap

module Availability {
  header "Availability.h"
  export *
}

//--- Availability.h

// CHECK:         @available(macOS 15.0, *)
// CHECK-NEXT:    func IntroducedInEachAppleOS()
void IntroducedInEachAppleOS()
  __attribute__((availability(macosx,introduced=15.0)))
  __attribute__((availability(ios,introduced=18.0)))
  __attribute__((availability(watchos,introduced=11.0)))
  __attribute__((availability(tvos,introduced=18.0)))
  __attribute__((availability(visionos,introduced=2.0)));

// CHECK:           @available(anyAppleOS 26.0, *)
// CHECK-NEXT:      func IntroducedInAnyAppleOS()
void IntroducedInAnyAppleOS()
  __attribute__((availability(anyappleos,introduced=26.0)));

// CHECK:           @available(macOS 15.0, *)
// CHECK-NEXT:      func IntroducedInAnyAppleOSAndMacOS()
void IntroducedInAnyAppleOSAndMacOS()
  __attribute__((availability(macosx,introduced=15.0)))
  __attribute__((availability(anyappleos,introduced=26.0)));

// CHECK:           @available(anyAppleOS, introduced: 26.0, deprecated: 26.2, obsoleted: 26.4)
// CHECK-NEXT:      func IntroducedDeprecatedAndObsoletedInAnyAppleOS()
void IntroducedDeprecatedAndObsoletedInAnyAppleOS()
  __attribute__((availability(anyappleos,introduced=26.0,deprecated=26.2,obsoleted=26.4)));

// CHECK:           @available(anyAppleOS, unavailable)
// CHECK-NEXT:      func UnavailableInAnyAppleOS()
void UnavailableInAnyAppleOS()
  __attribute__((availability(anyappleos,unavailable)));

//--- main.swift

import Availability
