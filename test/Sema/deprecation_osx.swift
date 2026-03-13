// RUN: %swift -typecheck -parse-as-library -target %target-cpu-apple-macosx51 %clang-importer-sdk -I %S/Inputs/custom-modules %s -verify
// RUN: %swift -typecheck -parse-as-library -target %target-cpu-apple-macosx51 %clang-importer-sdk -I %S/Inputs/custom-modules %s 2>&1 | %FileCheck %s '--implicit-check-not=<unknown>:0'
//
// This test requires a target of OS X 51 or later to test deprecation
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
  _ = NSDeprecatedOptions.first // expected-warning {{'NSDeprecatedOptions' was deprecated in macOS 51: Use a different API}}{{documentation-file=deprecated-declaration}}
  _ = NSDeprecatedEnum.first    // expected-warning {{'NSDeprecatedEnum' was deprecated in macOS 51: Use a different API}}{{documentation-file=deprecated-declaration}}
}

func useInSignature(options: NSDeprecatedOptions) { // expected-warning {{'NSDeprecatedOptions' was deprecated in macOS 51: Use a different API}}{{documentation-file=deprecated-declaration}}
}


class Super {
  @available(OSX, introduced: 10.9, deprecated: 51)
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

@available(OSX, introduced: 10.9, deprecated: 51)
func functionDeprecatedIn51() {
  _ = ClassDeprecatedIn51()
}

@available(OSX, introduced: 10.9, deprecated: 10.9)
class ClassDeprecatedIn10_9 {
}

@available(OSX, introduced: 10.8, deprecated: 51)
class ClassDeprecatedIn51 {
  var other51: ClassDeprecatedIn51 = ClassDeprecatedIn51()

  func usingDeprecatedIn10_9() {
    // Following clang, we don't warn here even though we are using a class
    // that was deprecated before the containing class was deprecated. The
    // policy is to not warn if the use is inside a declaration that
    // is deprecated on all deployment targets. We can revisit this policy
    // if needed.
    _ = ClassDeprecatedIn10_9()
  }
}

class ClassWithComputedPropertyDeprecatedIn51 {

  @available(OSX, introduced: 10.8, deprecated: 51)
  var annotatedPropertyDeprecatedIn51 : ClassDeprecatedIn51 {
    get {
      return ClassDeprecatedIn51()
    }
    set(newValue) {
      _ = ClassDeprecatedIn51()
    }
  }

  var unannotatedPropertyDeprecatedIn51 : ClassDeprecatedIn51 { // expected-warning {{ClassDeprecatedIn51' was deprecated in macOS 51}}{{documentation-file=deprecated-declaration}}
    get {
      return ClassDeprecatedIn51() // expected-warning {{ClassDeprecatedIn51' was deprecated in macOS 51}}{{documentation-file=deprecated-declaration}}
    }
    set(newValue) {
      _ = ClassDeprecatedIn51() // expected-warning {{ClassDeprecatedIn51' was deprecated in macOS 51}}{{documentation-file=deprecated-declaration}}
    }
  }

  var unannotatedStoredPropertyOfTypeDeprecatedIn51 : ClassDeprecatedIn51? = nil // expected-warning {{ClassDeprecatedIn51' was deprecated in macOS 51}}{{documentation-file=deprecated-declaration}}
}

func usesFunctionDeprecatedIn51() {
  _ = ClassDeprecatedIn51() // expected-warning {{ClassDeprecatedIn51' was deprecated in macOS 51}}{{documentation-file=deprecated-declaration}}
}

@available(OSX, introduced: 10.8, deprecated: 51)
func annotatedUsesFunctionDeprecatedIn51() {
  _ = ClassDeprecatedIn51()
}

func hasParameterDeprecatedIn51(p: ClassDeprecatedIn51) { // expected-warning {{ClassDeprecatedIn51' was deprecated in macOS 51}}{{documentation-file=deprecated-declaration}}
}

@available(OSX, introduced: 10.8, deprecated: 51)
func annotatedHasParameterDeprecatedIn51(p: ClassDeprecatedIn51) {
}

func hasReturnDeprecatedIn51() -> ClassDeprecatedIn51 { // expected-warning {{ClassDeprecatedIn51' was deprecated in macOS 51}}{{documentation-file=deprecated-declaration}}
}

@available(OSX, introduced: 10.8, deprecated: 51)
func annotatedHasReturnDeprecatedIn51() -> ClassDeprecatedIn51 {
}

var globalWithDeprecatedType : ClassDeprecatedIn51? = nil // expected-warning {{ClassDeprecatedIn51' was deprecated in macOS 51}}{{documentation-file=deprecated-declaration}}

@available(OSX, introduced: 10.8, deprecated: 51)
var annotatedGlobalWithDeprecatedType : ClassDeprecatedIn51?


enum EnumWithDeprecatedCasePayload {
  case WithDeprecatedPayload(p: ClassDeprecatedIn51) // expected-warning {{ClassDeprecatedIn51' was deprecated in macOS 51}}{{documentation-file=deprecated-declaration}}

  @available(OSX, introduced: 10.8, deprecated: 51)
  case AnnotatedWithDeprecatedPayload(p: ClassDeprecatedIn51)
}

extension ClassDeprecatedIn51 { // expected-warning {{'ClassDeprecatedIn51' was deprecated in macOS 51}}{{documentation-file=deprecated-declaration}}

}

@available(OSX, introduced: 10.8, deprecated: 51)
extension ClassDeprecatedIn51 {
  func methodInExtensionOfClassDeprecatedIn51() {
  }
}

func callMethodInDeprecatedExtension() {
  let o = ClassDeprecatedIn51() // expected-warning {{'ClassDeprecatedIn51' was deprecated in macOS 51}}{{documentation-file=deprecated-declaration}}

  o.methodInExtensionOfClassDeprecatedIn51() // expected-warning {{'methodInExtensionOfClassDeprecatedIn51()' was deprecated in macOS 51}}{{documentation-file=deprecated-declaration}}
}

func functionWithDeprecatedMethodInDeadElseBranch() {
  if #available(iOS 8.0, *) {
  } else {
    // This branch is dead on OS X, so we shouldn't emit a deprecation warning in it.
    let _ = ClassDeprecatedIn10_9()  // no-warning
  }

  if #available(OSX 10.9, *) { // no-warning
  } else {
    // This branch is dead because our minimum deployment target is 51.
    let _ = ClassDeprecatedIn10_9()  // no-warning
  }

  guard #available(iOS 8.0, *) else {
    // This branch is dead because our platform is OS X, so the wildcard always matches.
    let _ = ClassDeprecatedIn10_9()  // no-warning
  }
}

// https://github.com/apple/swift/issues/59843
class I59843_A {
  @available(macOS, deprecated: 51, renamed: "configure(with:)")
  static func configure(a: String, b: String) {}

  static func configure(with: Int) {}

  @available(macOS, deprecated: 51, renamed: "method(with:)")
  func method(a: String, b: String) {}

  func method(with: Int) {}

  func f() {
    self.method(a: "a", b: "b") // expected-warning{{'method(a:b:)' was deprecated in macOS 51: renamed to 'method(with:)'}}{{documentation-file=deprecated-declaration}}
    // expected-note@-1{{use 'method(with:)' instead}} {{none}} 
  }
}

class I59843_B {
  @available(macOS, deprecated: 51, renamed: "configure(with:and:)")
  static func configure(a: String, b: String) {}

  static func configure(with: Int, and: Int) {}
  
  @available(macOS, deprecated: 51, renamed: "method(with:and:)")
  func method(a: String, b: String) {}

  func method(with: Int, and: Int) {}

  // Context
  @available(macOS, deprecated: 51, renamed: "I59843_B.context(with:and:)")
  static func context(a: String, b: String) {}

  static func context(with: Int, and: Int) {}

  @available(macOS, deprecated: 51, renamed: "I59843_A.contextDiff(with:and:)")
  static func contextDiff(a: String, b: String) {}

  static func contextDiff(with: Int, and: Int) {}
  
  func f() {
    self.method(a: "a", b: "b") // expected-warning{{'method(a:b:)' was deprecated in macOS 51: renamed to 'method(with:and:)'}}{{documentation-file=deprecated-declaration}}
    // expected-note@-1{{use 'method(with:and:)' instead}} {{17-18=with}} {{25-26=and}}
  }
}

func I59843_f() {
  I59843_A.configure(a: "a", b: "b") // expected-warning{{'configure(a:b:)' was deprecated in macOS 51: renamed to 'configure(with:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1{{use 'configure(with:)' instead}} {{none}}
  I59843_B.configure(a: "a", b: "b") // expected-warning{{'configure(a:b:)' was deprecated in macOS 51: renamed to 'configure(with:and:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1{{use 'configure(with:and:)' instead}} {{22-23=with}} {{30-31=and}}
  I59843_B.context(a: "a", b: "b") // expected-warning{{'context(a:b:)' was deprecated in macOS 51: replaced by 'I59843_B.context(with:and:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1{{use 'I59843_B.context(with:and:)' instead}} {{20-21=with}} {{28-29=and}}
  I59843_B.contextDiff(a: "a", b: "b") // expected-warning{{'contextDiff(a:b:)' was deprecated in macOS 51: replaced by 'I59843_A.contextDiff(with:and:)'}}{{documentation-file=deprecated-declaration}}
  // expected-note@-1{{use 'I59843_A.contextDiff(with:and:)' instead}} {{3-23=I59843_A.contextDiff}} {{24-25=with}} {{32-33=and}}
}
