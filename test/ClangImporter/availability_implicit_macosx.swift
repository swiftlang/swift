// RUN: %swift -typecheck -verify -target %target-cpu-apple-macosx10.51 %clang-importer-sdk -I %S/Inputs/custom-modules %s %S/Inputs/availability_implicit_macosx_other.swift
// RUN: not %swift -typecheck -target %target-cpu-apple-macosx10.51 %clang-importer-sdk -I %S/Inputs/custom-modules %s %S/Inputs/availability_implicit_macosx_other.swift 2>&1 | %FileCheck %s '--implicit-check-not=<unknown>:0'

// REQUIRES: OS=macosx



// This is a temporary test for checking of availability diagnostics (explicit unavailability,
// deprecation, and potential unavailability) in synthesized code. After this checking
// is fully staged in, the tests in this file will be moved.
//

import Foundation

func useClassThatTriggersImportOfDeprecatedEnum() {
  // Check to make sure that the bodies of enum methods that are synthesized
  // when importing deprecated enums do not themselves trigger deprecation
  // warnings in the synthesized code.

  _ = NSClassWithDeprecatedOptionsInMethodSignature.sharedInstance()
}

func useClassThatTriggersImportOExplicitlyUnavailableOptions() {
  _ = NSClassWithPotentiallyUnavailableOptionsInMethodSignature.sharedInstance()
}

func useClassThatTriggersImportOfPotentiallyUnavailableOptions() {
  _ = NSClassWithExplicitlyUnavailableOptionsInMethodSignature.sharedInstance()
}

func directUseShouldStillTriggerDeprecationWarning() {
  _ = NSDeprecatedOptions.first // expected-warning {{'NSDeprecatedOptions' was deprecated in macOS 10.51: Use a different API}}
  _ = NSDeprecatedEnum.first    // expected-warning {{'NSDeprecatedEnum' was deprecated in macOS 10.51: Use a different API}}
}

func useInSignature(_ options: NSDeprecatedOptions) { // expected-warning {{'NSDeprecatedOptions' was deprecated in macOS 10.51: Use a different API}}
}

class SuperClassWithDeprecatedInitializer {
  @available(OSX, introduced: 10.9, deprecated: 10.51)
  init() { }
}

class SubClassWithSynthesizedDesignedInitializerOverride : SuperClassWithDeprecatedInitializer {
  // The synthesized designated initializer override calls super.init(), which is
  // deprecated, so the synthesized initializer is marked as deprecated as well.
  // This does not generate a warning here (perhaps it should?) but any call
  // to Sub's initializer will cause a deprecation warning.
}

func callImplicitInitializerOnSubClassWithSynthesizedDesignedInitializerOverride() {
  _ = SubClassWithSynthesizedDesignedInitializerOverride() // expected-warning {{'init()' was deprecated in macOS 10.51}}
}

@available(OSX, introduced: 10.9, deprecated: 10.51)
class NSDeprecatedSuperClass {
  var i : Int = 7 // Causes initializer to be synthesized
}

class NotDeprecatedSubClassOfDeprecatedSuperClass : NSDeprecatedSuperClass { // expected-warning {{'NSDeprecatedSuperClass' was deprecated in macOS 10.51}}
}

func callImplicitInitializerOnNotDeprecatedSubClassOfDeprecatedSuperClass() {
  // We do not expect a warning here because the synthesized initializer
  // in NotDeprecatedSubClassOfDeprecatedSuperClass is not itself marked
  // deprecated.
  _ = NotDeprecatedSubClassOfDeprecatedSuperClass()
}

@available(OSX, introduced: 10.9, deprecated: 10.51)
class NSDeprecatedSubClassOfDeprecatedSuperClass : NSDeprecatedSuperClass {
}

// Tests synthesis of materializeForSet
class ClassWithLimitedAvailabilityAccessors {
  var limitedGetter: Int {
    @available(OSX, introduced: 10.52)
    get { return 10 }
    set(newVal) {}
  }

  var limitedSetter: Int {
    get { return 10 }
    @available(OSX, introduced: 10.52)
    set(newVal) {}
  }
}

@available(*, unavailable)
func unavailableFunction() -> Int { return 10 } // expected-note 3{{'unavailableFunction()' has been explicitly marked unavailable here}}

class ClassWithReferencesLazyInitializers {
  var propWithUnavailableInInitializer: Int = unavailableFunction() // expected-error {{'unavailableFunction()' is unavailable}}

  lazy var lazyPropWithUnavailableInInitializer: Int = unavailableFunction() // expected-error {{'unavailableFunction()' is unavailable}}
}

@available(*, unavailable)
func unavailableUseInUnavailableFunction() {
  // Diagnose references to unavailable functions in non-implicit code
  // as errors
  unavailableFunction() // expected-error {{'unavailableFunction()' is unavailable}} expected-warning {{result of call to 'unavailableFunction()' is unused}}
}


@available(OSX 10.52, *)
func foo() {
  let _ =  SubOfOtherWithInit()
}
