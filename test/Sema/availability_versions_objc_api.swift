// RUN: %target-typecheck-verify-swift %clang-importer-sdk -I %S/Inputs/custom-modules
// RUN: not %target-swift-frontend -typecheck %clang-importer-sdk -I %S/Inputs/custom-modules %s 2>&1 | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import Foundation

// Tests for uses of version-based potential unavailability imported from ObjC APIs.
func callUnavailableObjC() {
  _ = NSAvailableOn10_51() // expected-error {{'NSAvailableOn10_51' is only available on OS X 10.51 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{add 'if #available' version check}}
  
  
  if #available(OSX 10.51, *) {
    let o = NSAvailableOn10_51()!
    
    // Properties
    _ = o.propertyOn10_52 // expected-error {{'propertyOn10_52' is only available on OS X 10.52 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}

    o.propertyOn10_52 = 22 // expected-error {{'propertyOn10_52' is only available on OS X 10.52 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}
    
    // Methods
    o.methodAvailableOn10_52() // expected-error {{'methodAvailableOn10_52()' is only available on OS X 10.52 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}
    
    // Initializers
    
    _ = NSAvailableOn10_51(stringOn10_52:"Hi") // expected-error {{'init(stringOn10_52:)' is only available on OS X 10.52 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}
  }
}

// Declarations with Objective-C-originated potentially unavailable APIs

func functionWithObjCParam(o: NSAvailableOn10_51) { // expected-error {{'NSAvailableOn10_51' is only available on OS X 10.51 or newer}}
    // expected-note@-1 {{add @available attribute to enclosing global function}}
}

class ClassExtendingUnvailableClass : NSAvailableOn10_51 { // expected-error {{'NSAvailableOn10_51' is only available on OS X 10.51 or newer}}
    // expected-note@-1 {{add @available attribute to enclosing class}}
}

// We allow classes to conform to potentially unavailable protocols
class ClassAdoptingUnavailableProtocol : NSProtocolAvailableOn10_51 {
}

class SomeSoonToBeConformingClass { }

extension SomeSoonToBeConformingClass : NSProtocolAvailableOn10_51 {
}

// Enums from Objective-C

let _: NSPotentiallyUnavailableOptions = .first // expected-error {{'NSPotentiallyUnavailableOptions' is only available on OS X 10.51 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}

let _: NSOptionsWithUnavailableElement = .third // expected-error {{'third' is only available on OS X 10.51 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}

let _: NSUnavailableEnum = .first // expected-error {{'NSUnavailableEnum' is only available on OS X 10.51 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}

let _: NSEnumWithUnavailableElement = .third // expected-error {{'third' is only available on OS X 10.51 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}

// Differing availability on getters and setters imported from ObjC.

func gettersAndSettersFromObjC(o: NSAvailableOn10_9) {
  let _: Int = o.propertyOn10_51WithSetterOn10_52After  // expected-error {{'propertyOn10_51WithSetterOn10_52After' is only available on OS X 10.51 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{add 'if #available' version check}}

  if #available(OSX 10.51, *) {
    // Properties with unavailable accessors declared before property in Objective-C header
    o.propertyOn10_51WithSetterOn10_52Before = 5 // expected-error {{setter for 'propertyOn10_51WithSetterOn10_52Before' is only available on OS X 10.52 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}

    let _: Int = o.propertyOn10_51WithGetterOn10_52Before // expected-error {{getter for 'propertyOn10_51WithGetterOn10_52Before' is only available on OS X 10.52 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}

    // Properties with unavailable accessors declared after property in Objective-C header
    o.propertyOn10_51WithSetterOn10_52After = 5 // expected-error {{setter for 'propertyOn10_51WithSetterOn10_52After' is only available on OS X 10.52 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}

    let _: Int = o.propertyOn10_51WithGetterOn10_52After // expected-error {{getter for 'propertyOn10_51WithGetterOn10_52After' is only available on OS X 10.52 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}

    // Property with unavailable setter redeclared in Objective-C category
    o.readOnlyRedeclaredWithSetterInCategory = 5 // expected-error {{setter for 'readOnlyRedeclaredWithSetterInCategory' is only available on OS X 10.52 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{add 'if #available' version check}}
  }
}

// Globals from Objective-C

func useGlobalsFromObjectiveC() {
  _ = globalStringAvailableOn10_51 // expected-error {{'globalStringAvailableOn10_51' is only available on OS X 10.51 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{add 'if #available' version check}}

  _ = globalStringAvailableOn10_52 // expected-error {{'globalStringAvailableOn10_52' is only available on OS X 10.52 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{add 'if #available' version check}}

  _ = globalClassInstanceAvailableOn10_51 // expected-error {{'globalClassInstanceAvailableOn10_51' is only available on OS X 10.51 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{add 'if #available' version check}}

  if #available(OSX 10.51, *) {
    _ = globalStringAvailableOn10_51
    let _: NSAvailableOn10_51 = globalClassInstanceAvailableOn10_51
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

  @available(OSX 10.51, *)
  @objc(doSomethingWithClass:)
  func doSomething(with k: AnnotatedFrameworkClass?) { }

  @available(OSX 10.51, *)
  @objc
  func doSomething(withNonNullableClass k: AnnotatedFrameworkClass) { }

  @available(OSX 10.51, *)
  @objc(doSomethingWithIUOClass:)
  func doSomething(withIUOClass k: AnnotatedFrameworkClass!) { }

  @objc
  @available(OSX 10.51, *)
  func returnSomething() -> AnnotatedFrameworkClass? {
    return nil
  }

  @objc
  func noUnavailableTypesInSignature() { }

  @objc(doSomethingWithClass:andLaterClass:) @available(OSX 10.52, *)
  func doSomething(with k: AnnotatedFrameworkClass, andLaterClass lk: AnnotatedLaterFrameworkClass) { }

  @objc
  @available(OSX 10.53, *)
  func someMethodWithAvailability() { }

  @available(OSX 10.51, *)
  @objc var someProperty: AnnotatedFrameworkClass {
    get { return AnnotatedFrameworkClass() }
    set(newValue) { }
  }
}

func callViaUnannotatedFrameworkProtocol(p: UnannotatedFrameworkProtocol) {
  let _ = p.returnSomething() // expected-error {{'returnSomething()' is only available on OS X 10.51 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{add 'if #available' version check}}
}

func callViaAnnotatedFrameworkProtocol(p: AnnotatedFrameworkProtocol) {
  // We won't synthesize availability for AnnotatedFrameworkProtocol because
  // the protocol has an availability annotation on it.
  let _ = p.returnSomething()
}

class SubclassOfFrameworkClassConformingToUnannotatedFrameworkProtocol : FrameworkClassConformingToUnannotatedFrameworkProtocol {
  @available(OSX 10.51, *)
  override func doSomething(withNonNullableClass k: AnnotatedFrameworkClass) {
  }

  @available(OSX 10.51, *)
  override var someProperty: AnnotatedFrameworkClass {
    get { return AnnotatedFrameworkClass() }
    set(newValue) { }
  }

  @available(OSX 10.52, *)
  override func doSomething(withIUOClass k: AnnotatedFrameworkClass!) { } // expected-error {{'doSomething' must be as available as declaration it overrides}}
}

@available(OSX 10.52, *)
class SubclassOfLaterFrameworkClassConformingToUnannotatedFrameworkProtocol : LaterFrameworkClassConformingToUnannotatedFrameworkProtocol {
  @available(OSX 10.52, *)
  override func doSomething(withNonNullableClass k: AnnotatedFrameworkClass) {
  }

  @available(OSX 10.53, *)
  override func someMethodWithAvailability() { }
}

class SubclassOfFrameworkClassConformingToLaterAnnotatedFrameworkProtocol : FrameworkClassConformingToLaterAnnotatedFrameworkProtocol {

  @available(OSX 10.52, *)
  override func returnSomething() -> AnnotatedFrameworkClass? {
  }

  @available(OSX 10.53, *)
  override func someMethodWithAvailability() { }

  @available(OSX 10.52, *)
  override var someProperty: AnnotatedFrameworkClass {
    get { return AnnotatedFrameworkClass() }
    set(newValue) { }
  }
}
