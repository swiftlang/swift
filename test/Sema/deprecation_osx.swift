// RUN: %swift -parse -parse-as-library -target x86_64-apple-macosx10.10 %clang-importer-sdk -I %S/Inputs/custom-modules - %s -verify
// RUN: %swift -parse -parse-as-library -target x86_64-apple-macosx10.10 %clang-importer-sdk -I %S/Inputs/custom-modules %s 2>&1 | FileCheck %s '--implicit-check-not=<unknown>:0'
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

  let _ = NSClassWithDeprecatedOptionsInMethodSignature.sharedInstance()
  let _ = NSClassWithExplicitlyUnavailableOptionsInMethodSignature.sharedInstance()
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

@availability(OSX, introduced=10.9, deprecated=10.10)
func functionDeprecatedIn10_10() {
  let _ = ClassDeprecatedIn10_10()
}

@availability(OSX, introduced=10.9, deprecated=10.9)
class ClassDeprecatedIn10_9 {
}

@availability(OSX, introduced=10.8, deprecated=10.10)
class ClassDeprecatedIn10_10 {
  var other10_10: ClassDeprecatedIn10_10 = ClassDeprecatedIn10_10()

  func usingDeprecatedIn10_9() {
    // Following clang, we don't warn here even though we are using a class
    // that was deprecated before the containing class was deprecated. The
    // policy is to not warn if the use is inside a declaration that
    // is deprecated on all deployment targets. We can revisit this policy
    // if needed.
    let _ = ClassDeprecatedIn10_9()
  }
}

class ClassWithComputedPropertyDeprecatedIn10_10 {

  @availability(OSX, introduced=10.8, deprecated=10.10)
  var annotatedPropertyDeprecatedIn10_10 : ClassDeprecatedIn10_10 {
    get {
      return ClassDeprecatedIn10_10()
    }
    set(newValue) {
      let _ = ClassDeprecatedIn10_10()
    }
  }

  // We really shouldn't be emitting three warnings here. It looks like
  // we are emitting one for each of the setter and getter, as well.
  var unannotatedPropertyDeprecatedIn10_10 : ClassDeprecatedIn10_10 { // expected-warning 3{{ClassDeprecatedIn10_10' was deprecated in OS X 10.10}}
    get {
      return ClassDeprecatedIn10_10() // expected-warning {{ClassDeprecatedIn10_10' was deprecated in OS X 10.10}}
    }
    set(newValue) {
      let _ = ClassDeprecatedIn10_10() // expected-warning {{ClassDeprecatedIn10_10' was deprecated in OS X 10.10}}
    }
  }

  var unannotatedStoredPropertyOfTypeDeprecatedIn10_10 : ClassDeprecatedIn10_10? = nil // expected-warning {{ClassDeprecatedIn10_10' was deprecated in OS X 10.10}}
}

func usesFunctionDeprecatedIn10_10() {
  let _ = ClassDeprecatedIn10_10() // expected-warning {{ClassDeprecatedIn10_10' was deprecated in OS X 10.10}}
}

@availability(OSX, introduced=10.8, deprecated=10.10)
func annotatedUsesFunctionDeprecatedIn10_10() {
  let _ = ClassDeprecatedIn10_10()
}

func hasParameterDeprecatedIn10_10(p: ClassDeprecatedIn10_10) { // expected-warning {{ClassDeprecatedIn10_10' was deprecated in OS X 10.10}}
}

@availability(OSX, introduced=10.8, deprecated=10.10)
func annotatedHasParameterDeprecatedIn10_10(p: ClassDeprecatedIn10_10) {
}

func hasReturnDeprecatedIn10_10() -> ClassDeprecatedIn10_10 { // expected-warning {{ClassDeprecatedIn10_10' was deprecated in OS X 10.10}}
}

@availability(OSX, introduced=10.8, deprecated=10.10)
func annotatedHasReturnDeprecatedIn10_10() -> ClassDeprecatedIn10_10 {
}

var globalWithDeprecatedType : ClassDeprecatedIn10_10? = nil // expected-warning {{ClassDeprecatedIn10_10' was deprecated in OS X 10.10}}

@availability(OSX, introduced=10.8, deprecated=10.10)
var annotatedGlobalWithDeprecatedType : ClassDeprecatedIn10_10? = nil


enum EnumWithDeprecatedCasePayload {
  case WithDeprecatedPayload(p: ClassDeprecatedIn10_10) // expected-warning {{ClassDeprecatedIn10_10' was deprecated in OS X 10.10}}

  @availability(OSX, introduced=10.8, deprecated=10.10)
  case AnnotatedWithDeprecatedPayload(p: ClassDeprecatedIn10_10)
}

extension ClassDeprecatedIn10_10 { // expected-warning {{'ClassDeprecatedIn10_10' was deprecated in OS X 10.10}}

}

@availability(OSX, introduced=10.8, deprecated=10.10)
extension ClassDeprecatedIn10_10 {
  func methodInExtensionOfClassDeprecatedIn10_10() {
  }
}

func callMethodInDeprecatedExtension() {
  let o = ClassDeprecatedIn10_10() // expected-warning {{'ClassDeprecatedIn10_10' was deprecated in OS X 10.10}}

  o.methodInExtensionOfClassDeprecatedIn10_10() // expected-warning {{'methodInExtensionOfClassDeprecatedIn10_10()' was deprecated in OS X 10.10}}
}
