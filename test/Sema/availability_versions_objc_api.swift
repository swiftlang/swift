// RUN: %target-parse-verify-swift %clang-importer-sdk -I %S/Inputs/custom-modules -enable-experimental-availability-checking

// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import Foundation

// Tests for uses of version-based potential unavailability imported from ObjC APIs.
func callUnavailableObjC() {
  let _ = NSAvailableOn10_10() // expected-error {{'NSAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @availability attribute to enclosing function}}
      // expected-note@-2 {{guard with version check}}
  
  
  if #os(OSX >= 10.10) {
    let o = NSAvailableOn10_10()
    
    // Properties
    let _ = o.propertyOn10_11 // expected-error {{'propertyOn10_11' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @availability attribute to enclosing function}}
        // expected-note@-2 {{guard with version check}}

    o.propertyOn10_11 = 22 // expected-error {{'propertyOn10_11' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @availability attribute to enclosing function}}
        // expected-note@-2 {{guard with version check}}
    
    // Methods
    o.methodAvailableOn10_11() // expected-error {{'methodAvailableOn10_11()' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @availability attribute to enclosing function}}
        // expected-note@-2 {{guard with version check}}
    
    // Initializers
    
    let _ = NSAvailableOn10_10(stringOn10_11:"Hi") // expected-error {{'init(stringOn10_11:)' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @availability attribute to enclosing function}}
        // expected-note@-2 {{guard with version check}}
  }
}

// Declarations with Objective-C-originated potentially unavailable APIs

func functionWithObjCParam(o: NSAvailableOn10_10) { // expected-error {{'NSAvailableOn10_10' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{add @availability attribute to enclosing function}}
}

class ClassExtendingUnvailableClass : NSAvailableOn10_10 { // expected-error {{'NSAvailableOn10_10' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{add @availability attribute to enclosing type}}
}

class ClassAdoptingUnavailableProtocol : NSProtocolAvailableOn10_10 { // expected-error {{'NSProtocolAvailableOn10_10' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{add @availability attribute to enclosing type}}
}

// Enums from Objective-C

let _: NSUnavailableOptions = .First // expected-error {{'NSUnavailableOptions' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{guard with version check}}

let _: NSOptionsWithUnavailableElement = .Third // expected-error {{'Third' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{guard with version check}}

let _: NSUnavailableEnum = .First // expected-error {{'NSUnavailableEnum' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{guard with version check}}

let _: NSEnumWithUnavailableElement = .Third // expected-error {{'Third' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{guard with version check}}

// Differing availability on getters and setters imported from ObjC.

func gettersAndSettersFromObjC(o: NSAvailableOn10_9) {
  let _: Int = o.propertyOn10_10WithSetterOn10_11After  // expected-error {{'propertyOn10_10WithSetterOn10_11After' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @availability attribute to enclosing function}}
      // expected-note@-2 {{guard with version check}}

  if #os(OSX >= 10.10) {
    // Properties with unavailable accessors declared before property in Objective-C header
    o.propertyOn10_10WithSetterOn10_11Before = 5 // expected-error {{setter for 'propertyOn10_10WithSetterOn10_11Before' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @availability attribute to enclosing function}}
        // expected-note@-2 {{guard with version check}}

    let _: Int = o.propertyOn10_10WithGetterOn10_11Before // expected-error {{getter for 'propertyOn10_10WithGetterOn10_11Before' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @availability attribute to enclosing function}}
        // expected-note@-2 {{guard with version check}}

    // Properties with unavailable accessors declared after property in Objective-C header
    o.propertyOn10_10WithSetterOn10_11After = 5 // expected-error {{setter for 'propertyOn10_10WithSetterOn10_11After' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @availability attribute to enclosing function}}
        // expected-note@-2 {{guard with version check}}

    let _: Int = o.propertyOn10_10WithGetterOn10_11After // expected-error {{getter for 'propertyOn10_10WithGetterOn10_11After' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @availability attribute to enclosing function}}
        // expected-note@-2 {{guard with version check}}

    // Property with unavailable setter redeclared in Objective-C category
    o.readOnlyRedeclaredWithSetterInCategory = 5 // expected-error {{setter for 'readOnlyRedeclaredWithSetterInCategory' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @availability attribute to enclosing function}}
        // expected-note@-2 {{guard with version check}}
  }
}
