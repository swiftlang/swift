// RUN: %swift -parse -target x86_64-apple-macosx10.10 %clang-importer-sdk -I %S/Inputs/custom-modules - %s -verify
// RUN: %swift -parse -target x86_64-apple-macosx10.10 %clang-importer-sdk -I %S/Inputs/custom-modules %s 2>&1 | FileCheck %s '--implicit-check-not=<unknown>:0'
//
// This test requires a target of OS X 10.10 or later to test deprecation
// diagnostics because (1) we only emit deprecation warnings if a symbol is
// deprecated on all deployment targets and (2) symbols deprecated on 10.9 and
// earlier are imported as unavailable.
//
// We run this test with FileCheck, as well, because the DiagnosticVerifier
// swallows diagnostics from buffers with unknown filenames, which is
// how diagnostics with invalid source locations appear. The
// --implicit-check-not checks to make sure we do not emit any such
// diagnostics with invalid source locations.

// REQUIRES: OS=macosx

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


class Super {
  @availability(OSX, introduced=10.9, deprecated=10.10)
  init() { }
}

class Sub : Super {
  // The synthesized call to super.init() calls a deprecated constructor, so we
  // really should emit a warning. We lost such a warning (with no source
  // location) as part of the quick fix for rdar://problem/20007266,
  // which involved spurious warnings in synthesized code.
  // rdar://problem/20024980 tracks adding a proper warning in this and similar
  /// cases.
}
