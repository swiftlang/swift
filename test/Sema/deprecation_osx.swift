// RUN: %swift -typecheck -parse-as-library -target %target-cpu-apple-macosx10.51 %clang-importer-sdk -I %S/Inputs/custom-modules %s -verify
// RUN: %swift -typecheck -parse-as-library -target %target-cpu-apple-macosx10.51 %clang-importer-sdk -I %S/Inputs/custom-modules %s 2>&1 | %FileCheck %s '--implicit-check-not=<unknown>:0'
//
// This test requires a target of OS X 10.51 or later to test deprecation
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

  _ = NSClassWithDeprecatedOptionsInMethodSignature.sharedInstance()
  _ = NSClassWithExplicitlyUnavailableOptionsInMethodSignature.sharedInstance()
}

func directUseShouldStillTriggerDeprecationWarning() {
  _ = NSDeprecatedOptions.first // expected-warning {{'NSDeprecatedOptions' was deprecated in macOS 10.51: Use a different API}}
  _ = NSDeprecatedEnum.first    // expected-warning {{'NSDeprecatedEnum' was deprecated in macOS 10.51: Use a different API}}
}

func useInSignature(options: NSDeprecatedOptions) { // expected-warning {{'NSDeprecatedOptions' was deprecated in macOS 10.51: Use a different API}}
}


class Super {
  @available(OSX, introduced: 10.9, deprecated: 10.51)
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

@available(OSX, introduced: 10.9, deprecated: 10.51)
func functionDeprecatedIn10_51() {
  _ = ClassDeprecatedIn10_51()
}

@available(OSX, introduced: 10.9, deprecated: 10.9)
class ClassDeprecatedIn10_9 {
}

@available(OSX, introduced: 10.8, deprecated: 10.51)
class ClassDeprecatedIn10_51 {
  var other10_51: ClassDeprecatedIn10_51 = ClassDeprecatedIn10_51()

  func usingDeprecatedIn10_9() {
    // Following clang, we don't warn here even though we are using a class
    // that was deprecated before the containing class was deprecated. The
    // policy is to not warn if the use is inside a declaration that
    // is deprecated on all deployment targets. We can revisit this policy
    // if needed.
    _ = ClassDeprecatedIn10_9()
  }
}

class ClassWithComputedPropertyDeprecatedIn10_51 {

  @available(OSX, introduced: 10.8, deprecated: 10.51)
  var annotatedPropertyDeprecatedIn10_51 : ClassDeprecatedIn10_51 {
    get {
      return ClassDeprecatedIn10_51()
    }
    set(newValue) {
      _ = ClassDeprecatedIn10_51()
    }
  }

  var unannotatedPropertyDeprecatedIn10_51 : ClassDeprecatedIn10_51 { // expected-warning {{ClassDeprecatedIn10_51' was deprecated in macOS 10.51}}
    get {
      return ClassDeprecatedIn10_51() // expected-warning {{ClassDeprecatedIn10_51' was deprecated in macOS 10.51}}
    }
    set(newValue) {
      _ = ClassDeprecatedIn10_51() // expected-warning {{ClassDeprecatedIn10_51' was deprecated in macOS 10.51}}
    }
  }

  var unannotatedStoredPropertyOfTypeDeprecatedIn10_51 : ClassDeprecatedIn10_51? = nil // expected-warning {{ClassDeprecatedIn10_51' was deprecated in macOS 10.51}}
}

func usesFunctionDeprecatedIn10_51() {
  _ = ClassDeprecatedIn10_51() // expected-warning {{ClassDeprecatedIn10_51' was deprecated in macOS 10.51}}
}

@available(OSX, introduced: 10.8, deprecated: 10.51)
func annotatedUsesFunctionDeprecatedIn10_51() {
  _ = ClassDeprecatedIn10_51()
}

func hasParameterDeprecatedIn10_51(p: ClassDeprecatedIn10_51) { // expected-warning {{ClassDeprecatedIn10_51' was deprecated in macOS 10.51}}
}

@available(OSX, introduced: 10.8, deprecated: 10.51)
func annotatedHasParameterDeprecatedIn10_51(p: ClassDeprecatedIn10_51) {
}

func hasReturnDeprecatedIn10_51() -> ClassDeprecatedIn10_51 { // expected-warning {{ClassDeprecatedIn10_51' was deprecated in macOS 10.51}}
}

@available(OSX, introduced: 10.8, deprecated: 10.51)
func annotatedHasReturnDeprecatedIn10_51() -> ClassDeprecatedIn10_51 {
}

var globalWithDeprecatedType : ClassDeprecatedIn10_51? = nil // expected-warning {{ClassDeprecatedIn10_51' was deprecated in macOS 10.51}}

@available(OSX, introduced: 10.8, deprecated: 10.51)
var annotatedGlobalWithDeprecatedType : ClassDeprecatedIn10_51?


enum EnumWithDeprecatedCasePayload {
  case WithDeprecatedPayload(p: ClassDeprecatedIn10_51) // expected-warning {{ClassDeprecatedIn10_51' was deprecated in macOS 10.51}}

  @available(OSX, introduced: 10.8, deprecated: 10.51)
  case AnnotatedWithDeprecatedPayload(p: ClassDeprecatedIn10_51)
}

extension ClassDeprecatedIn10_51 { // expected-warning {{'ClassDeprecatedIn10_51' was deprecated in macOS 10.51}}

}

@available(OSX, introduced: 10.8, deprecated: 10.51)
extension ClassDeprecatedIn10_51 {
  func methodInExtensionOfClassDeprecatedIn10_51() {
  }
}

func callMethodInDeprecatedExtension() {
  let o = ClassDeprecatedIn10_51() // expected-warning {{'ClassDeprecatedIn10_51' was deprecated in macOS 10.51}}

  o.methodInExtensionOfClassDeprecatedIn10_51() // expected-warning {{'methodInExtensionOfClassDeprecatedIn10_51()' was deprecated in macOS 10.51}}
}

func functionWithDeprecatedMethodInDeadElseBranch() {
  if #available(iOS 8.0, *) {
  } else {
    // This branch is dead on OS X, so we shouldn't emit a deprecation warning in it.
    let _ = ClassDeprecatedIn10_9()  // no-warning
  }

  if #available(OSX 10.9, *) { // no-warning
  } else {
    // This branch is dead because our minimum deployment target is 10.51.
    let _ = ClassDeprecatedIn10_9()  // no-warning
  }

  guard #available(iOS 8.0, *) else {
    // This branch is dead because our platform is OS X, so the wildcard always matches.
    let _ = ClassDeprecatedIn10_9()  // no-warning
  }
}
