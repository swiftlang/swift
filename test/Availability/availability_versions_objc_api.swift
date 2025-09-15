// RUN: %target-typecheck-verify-swift %clang-importer-sdk -I %S/Inputs/custom-modules
// RUN: not %target-swift-frontend -typecheck %clang-importer-sdk -I %S/Inputs/custom-modules %s 2>&1 | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import Foundation

// Tests for uses of version-based potential unavailability imported from ObjC APIs.
func callUnavailableObjC() {
      // expected-note@-1 5{{add '@available' attribute to enclosing global function}}
  _ = NSAvailableOn51() // expected-error {{'NSAvailableOn51' is only available in macOS 51 or newer}}
      // expected-note@-1 {{add 'if #available' version check}}
  
  
  if #available(OSX 51, *) {
    let o = NSAvailableOn51()!
    
    // Properties
    _ = o.propertyOn52 // expected-error {{'propertyOn52' is only available in macOS 52 or newer}}
        // expected-note@-1 {{add 'if #available' version check}}

    o.propertyOn52 = 22 // expected-error {{'propertyOn52' is only available in macOS 52 or newer}}
        // expected-note@-1 {{add 'if #available' version check}}
    
    // Methods
    o.methodAvailableOn52() // expected-error {{'methodAvailableOn52()' is only available in macOS 52 or newer}}
        // expected-note@-1 {{add 'if #available' version check}}
    
    // Initializers
    
    _ = NSAvailableOn51(stringOn52:"Hi") // expected-error {{'init(stringOn52:)' is only available in macOS 52 or newer}}
        // expected-note@-1 {{add 'if #available' version check}}
  }
}

// Declarations with Objective-C-originated potentially unavailable APIs

func functionWithObjCParam(o: NSAvailableOn51) { // expected-error {{'NSAvailableOn51' is only available in macOS 51 or newer}}
    // expected-note@-1 {{add '@available' attribute to enclosing global function}}
}

class ClassExtendingUnvailableClass : NSAvailableOn51 { // expected-error {{'NSAvailableOn51' is only available in macOS 51 or newer}}
    // expected-note@-1 {{add '@available' attribute to enclosing class}}
}

// We allow classes to conform to potentially unavailable protocols
class ClassAdoptingUnavailableProtocol : NSProtocolAvailableOn51 {
}

class SomeSoonToBeConformingClass { }

extension SomeSoonToBeConformingClass : NSProtocolAvailableOn51 {
}

// Enums from Objective-C

let _: NSPotentiallyUnavailableOptions = .first // expected-error {{'NSPotentiallyUnavailableOptions' is only available in macOS 51 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}

let _: NSOptionsWithUnavailableElement = .third // expected-error {{'third' is only available in macOS 51 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}

let _: NSUnavailableEnum = .first // expected-error {{'NSUnavailableEnum' is only available in macOS 51 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}

let _: NSEnumWithUnavailableElement = .third // expected-error {{'third' is only available in macOS 51 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}

// Differing availability on getters and setters imported from ObjC.

func gettersAndSettersFromObjC(o: NSAvailableOn10_9) {
      // expected-note@-1 6{{add '@available' attribute to enclosing global function}}
  let _: Int = o.propertyOn51WithSetterOn52After  // expected-error {{'propertyOn51WithSetterOn52After' is only available in macOS 51 or newer}}
      // expected-note@-1 {{add 'if #available' version check}}

  if #available(OSX 51, *) {
    // Properties with unavailable accessors declared before property in Objective-C header
    o.propertyOn51WithSetterOn52Before = 5 // expected-error {{setter for 'propertyOn51WithSetterOn52Before' is only available in macOS 52 or newer}}
        // expected-note@-1 {{add 'if #available' version check}}

    let _: Int = o.propertyOn51WithGetterOn52Before // expected-error {{getter for 'propertyOn51WithGetterOn52Before' is only available in macOS 52 or newer}}
        // expected-note@-1 {{add 'if #available' version check}}

    // Properties with unavailable accessors declared after property in Objective-C header
    o.propertyOn51WithSetterOn52After = 5 // expected-error {{setter for 'propertyOn51WithSetterOn52After' is only available in macOS 52 or newer}}
        // expected-note@-1 {{add 'if #available' version check}}

    let _: Int = o.propertyOn51WithGetterOn52After // expected-error {{getter for 'propertyOn51WithGetterOn52After' is only available in macOS 52 or newer}}
        // expected-note@-1 {{add 'if #available' version check}}

    // Property with unavailable setter redeclared in Objective-C category
    o.readOnlyRedeclaredWithSetterInCategory = 5 // expected-error {{setter for 'readOnlyRedeclaredWithSetterInCategory' is only available in macOS 52 or newer}}
        // expected-note@-1 {{add 'if #available' version check}}
  }
}

// Globals from Objective-C

func useGlobalsFromObjectiveC() {
      // expected-note@-1 3{{add '@available' attribute to enclosing global function}}
  _ = globalStringAvailableOn51 // expected-error {{'globalStringAvailableOn51' is only available in macOS 51 or newer}}
      // expected-note@-1 {{add 'if #available' version check}}

  _ = globalStringAvailableOn52 // expected-error {{'globalStringAvailableOn52' is only available in macOS 52 or newer}}
      // expected-note@-1 {{add 'if #available' version check}}

  _ = globalClassInstanceAvailableOn51 // expected-error {{'globalClassInstanceAvailableOn51' is only available in macOS 51 or newer}}
      // expected-note@-1 {{add 'if #available' version check}}

  if #available(OSX 51, *) {
    _ = globalStringAvailableOn51
    let _: NSAvailableOn51 = globalClassInstanceAvailableOn51
  }
}

// Optional Protocol Requirements from Objective-C

// Make sure we're not emitting errors in the Foundation module, where the witness is.
// CHECK-NOT: Foundation.ClassWithMethodFromNSProtocolWithOptionalRequirement:
class SubclassOfNSClassWithMethodFromNSProtocolWithOptionalRequirement : NSClassWithMethodFromNSProtocolWithOptionalRequirement {

}

class SubclassWithItsOwnAvailableWitnessOfNSClassWithMethodFromNSProtocolWithOptionalRequirement : NSClassWithMethodFromNSProtocolWithOptionalRequirement {
  override func optionalRequirement() { }
}

// Inference of protocol requirement availability when checking conformance to
// unannotated Objective-C protocols
class UserClass : UnannotatedFrameworkProtocol {

  @available(OSX 51, *)
  @objc(doSomethingWithClass:)
  func doSomething(with k: AnnotatedFrameworkClass?) { }

  @available(OSX 51, *)
  @objc
  func doSomething(withNonNullableClass k: AnnotatedFrameworkClass) { }

  @available(OSX 51, *)
  @objc(doSomethingWithIUOClass:)
  func doSomething(withIUOClass k: AnnotatedFrameworkClass!) { }

  @objc
  @available(OSX 51, *)
  func returnSomething() -> AnnotatedFrameworkClass? {
    return nil
  }

  @objc
  func noUnavailableTypesInSignature() { }

  @objc(doSomethingWithClass:andLaterClass:) @available(OSX 52, *)
  func doSomething(with k: AnnotatedFrameworkClass, andLaterClass lk: AnnotatedLaterFrameworkClass) { }

  @objc
  @available(OSX 53, *)
  func someMethodWithAvailability() { }

  @available(OSX 51, *)
  @objc var someProperty: AnnotatedFrameworkClass {
    get { return AnnotatedFrameworkClass() }
    set(newValue) { }
  }
}

func callViaUnannotatedFrameworkProtocol(p: UnannotatedFrameworkProtocol) {
      // expected-note@-1 {{add '@available' attribute to enclosing global function}}
  let _ = p.returnSomething() // expected-error {{'returnSomething()' is only available in macOS 51 or newer}}
      // expected-note@-1 {{add 'if #available' version check}}
}

func callViaAnnotatedFrameworkProtocol(p: AnnotatedFrameworkProtocol) {
  // We won't synthesize availability for AnnotatedFrameworkProtocol because
  // the protocol has an availability annotation on it.
  let _ = p.returnSomething()
}

class SubclassOfFrameworkClassConformingToUnannotatedFrameworkProtocol : FrameworkClassConformingToUnannotatedFrameworkProtocol {
  @available(OSX 51, *)
  override func doSomething(withNonNullableClass k: AnnotatedFrameworkClass) {
  }

  @available(OSX 51, *)
  override var someProperty: AnnotatedFrameworkClass {
    get { return AnnotatedFrameworkClass() }
    set(newValue) { }
  }

  @available(OSX 52, *)
  override func doSomething(withIUOClass k: AnnotatedFrameworkClass!) { } // expected-error {{'doSomething' must be as available as declaration it overrides}}
}

@available(OSX 52, *)
class SubclassOfLaterFrameworkClassConformingToUnannotatedFrameworkProtocol : LaterFrameworkClassConformingToUnannotatedFrameworkProtocol {
  @available(OSX 52, *)
  override func doSomething(withNonNullableClass k: AnnotatedFrameworkClass) {
  }

  @available(OSX 53, *)
  override func someMethodWithAvailability() { }
}

class SubclassOfFrameworkClassConformingToLaterAnnotatedFrameworkProtocol : FrameworkClassConformingToLaterAnnotatedFrameworkProtocol {

  @available(OSX 52, *)
  override func returnSomething() -> AnnotatedFrameworkClass? {
  }

  @available(OSX 53, *)
  override func someMethodWithAvailability() { }

  @available(OSX 52, *)
  override var someProperty: AnnotatedFrameworkClass {
    get { return AnnotatedFrameworkClass() }
    set(newValue) { }
  }
}
