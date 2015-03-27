// RUN: %swift -parse -verify -target x86_64-apple-macosx10.10 -enable-availability-checking-in-implicit-functions %clang-importer-sdk -I %S/Inputs/custom-modules %s
// RUN: %swift -parse -target x86_64-apple-macosx10.10 -enable-availability-checking-in-implicit-functions %clang-importer-sdk -I %S/Inputs/custom-modules %s 2>&1 | FileCheck %s '--implicit-check-not=<unknown>:0'

// REQUIRES: OS=macosx

// XFAIL: *

// This is a temporary test for checking of availability diagnostics (explicit unavailability,
// deprecation, and potential unavailability) in synthesized code. After this checking
// is fully staged in, the tests in this file will be moved.
//

import Foundation

func useClassThatTriggersImportOfDeprecatedEnum() {
  // Check to make sure that the bodies of enum methods that are synthesized
  // when importing deprecated enums do not themselves trigger deprecation
  // warnings in the synthesized code.

  let _ = NSClassWithOptionsInMethodSignature.sharedInstance()
}

func directUseShouldStillTriggerDeprecationWarning() {
  let _ = NSDeprecatedOptions.First // expected-warning {{'NSDeprecatedOptions' was deprecated in OS X 10.10: Use a different API}}
  let _ = NSDeprecatedEnum.First    // expected-warning {{'NSDeprecatedEnum' was deprecated in OS X 10.10: Use a different API}}
}

func useInSignature(options: NSDeprecatedOptions) { // expected-warning {{'NSDeprecatedOptions' was deprecated in OS X 10.10: Use a different API}}
}
