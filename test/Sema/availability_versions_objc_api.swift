// RUN: %target-parse-verify-swift %clang-importer-sdk -I %S/Inputs/custom-modules
// RUN: not %target-swift-frontend -parse %clang-importer-sdk -I %S/Inputs/custom-modules %s 2>&1 | FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import Foundation

// Tests for uses of version-based potential unavailability imported from ObjC APIs.
func callUnavailableObjC() {
  _ = NSAvailableOn10_10() // expected-error {{'NSAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{add 'if #available' version check}}
  
  
  if #available(OSX 10.10, *) {
    let o = NSAvailableOn10_10()
    
    // Properties
    _ = o.propertyOn10_11 // expected-error {{'propertyOn10_11' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}

    o.propertyOn10_11 = 22 // expected-error {{'propertyOn10_11' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}
    
    // Methods
    o.methodAvailableOn10_11() // expected-error {{'methodAvailableOn10_11()' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}
    
    // Initializers
    
    _ = NSAvailableOn10_10(stringOn10_11:"Hi") // expected-error {{'init(stringOn10_11:)' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}
  }
}

// Declarations with Objective-C-originated potentially unavailable APIs

func functionWithObjCParam(o: NSAvailableOn10_10) { // expected-error {{'NSAvailableOn10_10' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{add @available attribute to enclosing global function}}
}

class ClassExtendingUnvailableClass : NSAvailableOn10_10 { // expected-error {{'NSAvailableOn10_10' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{add @available attribute to enclosing class}}
}

// We allow classes to conform to potentially unavailable protocols
class ClassAdoptingUnavailableProtocol : NSProtocolAvailableOn10_10 {
}

class SomeSoonToBeConformingClass { }

extension SomeSoonToBeConformingClass : NSProtocolAvailableOn10_10 {
}

// Enums from Objective-C

let _: NSPotentiallyUnavailableOptions = .First // expected-error {{'NSPotentiallyUnavailableOptions' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}

let _: NSOptionsWithUnavailableElement = .Third // expected-error {{'Third' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}

let _: NSUnavailableEnum = .First // expected-error {{'NSUnavailableEnum' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}

let _: NSEnumWithUnavailableElement = .Third // expected-error {{'Third' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}

// Differing availability on getters and setters imported from ObjC.

func gettersAndSettersFromObjC(o: NSAvailableOn10_9) {
  let _: Int = o.propertyOn10_10WithSetterOn10_11After  // expected-error {{'propertyOn10_10WithSetterOn10_11After' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{add 'if #available' version check}}

  if #available(OSX 10.10, *) {
    // Properties with unavailable accessors declared before property in Objective-C header
    o.propertyOn10_10WithSetterOn10_11Before = 5 // expected-error {{setter for 'propertyOn10_10WithSetterOn10_11Before' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}

    let _: Int = o.propertyOn10_10WithGetterOn10_11Before // expected-error {{getter for 'propertyOn10_10WithGetterOn10_11Before' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}

    // Properties with unavailable accessors declared after property in Objective-C header
    o.propertyOn10_10WithSetterOn10_11After = 5 // expected-error {{setter for 'propertyOn10_10WithSetterOn10_11After' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}

    let _: Int = o.propertyOn10_10WithGetterOn10_11After // expected-error {{getter for 'propertyOn10_10WithGetterOn10_11After' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}

    // Property with unavailable setter redeclared in Objective-C category
    o.readOnlyRedeclaredWithSetterInCategory = 5 // expected-error {{setter for 'readOnlyRedeclaredWithSetterInCategory' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}
  }
}

// Globals from Objective-C

func useGlobalsFromObjectiveC() {
  _ = globalStringAvailableOn10_10 // expected-error {{'globalStringAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{add 'if #available' version check}}

  _ = globalStringAvailableOn10_11 // expected-error {{'globalStringAvailableOn10_11' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{add 'if #available' version check}}

  _ = globalClassInstanceAvailableOn10_10 // expected-error {{'globalClassInstanceAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{add 'if #available' version check}}

  if #available(OSX 10.10, *) {
    _ = globalStringAvailableOn10_10
    let _: NSAvailableOn10_10 = globalClassInstanceAvailableOn10_10
  }
}

// Optional Protocol Requirements from Objective-C

// Make sure we're not emitting errors in the Foundation module, where the witness is.
// CHECK-NOT: Foundation.NSClassWithMethodFromNSProtocolWithOptionalRequirement:
class SubclassOfNSClassWithMethodFromNSProtocolWithOptionalRequirement : NSClassWithMethodFromNSProtocolWithOptionalRequirement {

}

class SubclassWithItsOwnAvailableWitnessOfNSClassWithMethodFromNSProtocolWithOptionalRequirement : NSClassWithMethodFromNSProtocolWithOptionalRequirement {
  override func optionalRequirement() { }
}

// Inference of protocol requirement availability when checking conformance to
// unannotated Objective-C protocols
class UserClass : UnannotatedFrameworkProtocol {

  @available(OSX 10.10, *)
  @objc
  func doSomethingWithClass(k: AnnotatedFrameworkClass?) { }

  @available(OSX 10.10, *)
  @objc
  func doSomethingWithNonNullableClass(k: AnnotatedFrameworkClass) { }

  @available(OSX 10.10, *)
  @objc
  func doSomethingWithIUOClass(k: AnnotatedFrameworkClass!) { }

  @objc
  @available(OSX 10.10, *)
  func returnSomething() -> AnnotatedFrameworkClass? {
    return nil
  }

  @objc
  func noUnavailableTypesInSignature() { }

  @objc @available(OSX 10.11, *)
  func doSomethingWithClass(k: AnnotatedFrameworkClass, andLaterClass lk: AnnotatedLaterFrameworkClass) { }

  @objc
  @available(OSX 10.12, *)
  func someMethodWithAvailability() { }

  @available(OSX 10.10, *)
  @objc var someProperty: AnnotatedFrameworkClass {
    get { return AnnotatedFrameworkClass() }
    set(newValue) { }
  }
}

func callViaUnannotatedFrameworkProtocol(p: UnannotatedFrameworkProtocol) {
  let _ = p.returnSomething() // expected-error {{'returnSomething()' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{add 'if #available' version check}}
}

func callViaAnnotatedFrameworkProtocol(p: AnnotatedFrameworkProtocol) {
  // We won't synthesize availability for AnnotatedFrameworkProtocol because
  // the protocol has an availability annotation on it.
  let _ = p.returnSomething()
}

class SubclassOfFrameworkClassConformingToUnannotatedFrameworkProtocol : FrameworkClassConformingToUnannotatedFrameworkProtocol {
  @available(OSX 10.10, *)
  override func doSomethingWithNonNullableClass(k: AnnotatedFrameworkClass) {
  }

  @available(OSX 10.10, *)
  override var someProperty: AnnotatedFrameworkClass {
    get { return AnnotatedFrameworkClass() }
    set(newValue) { }
  }

  @available(OSX 10.11, *)
  override func doSomethingWithIUOClass(k: AnnotatedFrameworkClass!) { } // expected-error {{'doSomethingWithIUOClass' must be as available as declaration it overrides}}
}

@available(OSX 10.11, *)
class SubclassOfLaterFrameworkClassConformingToUnannotatedFrameworkProtocol : LaterFrameworkClassConformingToUnannotatedFrameworkProtocol {
  @available(OSX 10.11, *)
  override func doSomethingWithNonNullableClass(k: AnnotatedFrameworkClass) {
  }

  @available(OSX 10.12, *)
  override func someMethodWithAvailability() { }
}

class SubclassOfFrameworkClassConformingToLaterAnnotatedFrameworkProtocol : FrameworkClassConformingToLaterAnnotatedFrameworkProtocol {

  @available(OSX 10.11, *)
  override func returnSomething() -> AnnotatedFrameworkClass? {
  }

  @available(OSX 10.12, *)
  override func someMethodWithAvailability() { }

  @available(OSX 10.11, *)
  override var someProperty: AnnotatedFrameworkClass {
    get { return AnnotatedFrameworkClass() }
    set(newValue) { }
  }
}
