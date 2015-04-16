// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify -I %S/Inputs/custom-modules %s

// REQUIRES: objc_interop

import Dispatch
import Foundation
import stdio
import AvailabilityExtras

// Test if an instance method marked __attribute__((unavailable)) on
// the *class* NSObject can be used.
func test_unavailable_instance_method(x : NSObject) -> Bool {
  return x.allowsWeakReference() // expected-error {{'allowsWeakReference()' is unavailable}}
}

func test_unavailable_method_in_protocol(x : NSObjectProtocol) {
  x.retain() // expected-error {{'retain()' is unavailable}}
}
func test_unavailable_method_in_protocol_use_class_instance(x : NSObject) {
  x.retain() // expected-error {{'retain()' is unavailable}}
}

func test_unavailable_func(x : NSObject) {
  NSDeallocateObject(x) // expected-error {{'NSDeallocateObject' is unavailable}}
}

func test_deprecated_imported_as_unavailable(s:UnsafeMutablePointer<CChar>) {
  let x = tmpnam(s) // expected-warning {{'tmpnam' is deprecated: Due to security concerns inherent in the design of tmpnam(3), it is highly recommended that you use mkstemp(3) instead.}}
}

func test_NSInvocation(x: NSInvocation,         // expected-error {{'NSInvocation' is unavailable}}
                       y: NSInvocationOperation,// expected-error {{'NSInvocationOperation' is unavailable}}
                       z: NSMethodSignature) {} // expected-error {{'NSMethodSignature' is unavailable}}

func test_class_avail(x:NSObject, obj: AnyObject) {
  x.`class`() // expected-error {{'class()' is unavailable: use 'dynamicType' instead}}
  NSObject.`class`() // expected-error {{'class()' is unavailable: use 'self' instead}}
  obj.`class`!() // expected-error {{'class()' is unavailable: use 'dynamicType' instead}}
}

func test_unavailable_app_extension() {
  // No expected error here.  See corresponding App extension test.
  println(SomeCrazyAppExtensionForbiddenAPI()) // no-error
}

func test_swift_unavailable() {
  NSSwiftOldUnavailableFunction() // expected-error {{'NSSwiftOldUnavailableFunction()' is unavailable in Swift}}
  NSSwiftNewUnavailableFunction() // expected-error {{'NSSwiftNewUnavailableFunction()' is unavailable in Swift}}
  NSSwiftNewUnavailableFunction2() // expected-error {{'NSSwiftNewUnavailableFunction2()' is unavailable in Swift}}
  NSSwiftNewUnavailableFunctionPremium() // expected-error {{'NSSwiftNewUnavailableFunctionPremium()' is unavailable in Swift: You didn't want to use it anyway.}}

  let x: NSSwiftUnavailableStruct? = nil // expected-error {{'NSSwiftUnavailableStruct' is unavailable in Swift}}
}

func test_CFReleaseRetainAutorelease(x : CFTypeRef, color : CGColorRef ) {
  CFRelease(x)              // expected-error {{'CFRelease' is unavailable: Core Foundation objects are automatically memory managed}}
  CGColorRelease(color)     // expected-error {{'CGColorRelease' is unavailable: Core Foundation objects are automatically memory managed}}
  CFRetain(x)               // expected-error {{'CFRetain' is unavailable: Core Foundation objects are automatically memory managed}}
  CGColorRetain(color)      // expected-error {{'CGColorRetain' is unavailable: Core Foundation objects are automatically memory managed}}
  CFAutorelease(x)          // expected-error {{'CFAutorelease' is unavailable: Core Foundation objects are automatically memory managed}}
}

func testRedeclarations() {
  unavail1() // expected-error {{is unavailable: first}}
  unavail2() // expected-error {{is unavailable: middle}}
  unavail3() // expected-error {{is unavailable: last}}

  UnavailClass1.self // expected-error {{is unavailable: first}}
  UnavailClass2.self // expected-error {{is unavailable: middle}}
  UnavailClass3.self // expected-error {{is unavailable: last}}

  let _: UnavailStruct1 // expected-error {{is unavailable: first}}
  let _: UnavailStruct2 // expected-error {{is unavailable: first}}
  let _: UnavailStruct3 // expected-error {{is unavailable: first}}
  let _: UnavailStruct4 // expected-error {{is unavailable: middle}}
  let _: UnavailStruct5 // expected-error {{is unavailable: middle}}
  let _: UnavailStruct6 // expected-error {{is unavailable: last}}

  let _: UnavailProto1 // expected-error {{is unavailable: first}}
  let _: UnavailProto2 // expected-error {{is unavailable: middle}}
  let _: UnavailProto3 // expected-error {{is unavailable: last}}
}

func test_NSZone(z : NSZone) { 
  NSCreateZone(1, 1, true)  // expected-error {{'NSCreateZone' is unavailable}}
  NSSetZoneName(z, "name")  // expected-error {{'NSSetZoneName' is unavailable}}
  NSZoneName(z)             // expected-error {{'NSZoneName' is unavailable}}
}

func test_DistributedObjects(o: NSObject,
                             a: NSConnection,           // expected-error {{'NSConnection' is unavailable: you may be able to use XPC instead}}
                             b: NSConnectionDelegate,   // expected-error {{'NSConnectionDelegate' is unavailable: you may be able to use XPC instead}}
                             c: NSDistantObjectRequest, // expected-error {{'NSDistantObjectRequest' is unavailable: you may be able to use XPC instead}}
                             d: NSDistantObject,        // expected-error {{'NSDistantObject' is unavailable: you may be able to use XPC instead}}
                             e: NSPortNameServer,       // expected-error {{'NSPortNameServer' is unavailable: you may be able to use XPC instead}}
                             f: NSMachBootstrapServer,  // expected-error {{'NSMachBootstrapServer' is unavailable: you may be able to use XPC instead}}
                             g: NSMessagePortNameServer, // expected-error {{'NSMessagePortNameServer' is unavailable: you may be able to use XPC instead}}
                             h: NSSocketPortNameServer, // expected-error {{'NSSocketPortNameServer' is unavailable: you may be able to use XPC instead}}
                             i: NSPortCoder) {          // expected-error {{'NSPortCoder' is unavailable: you may be able to use XPC instead}}

  let ca = NSConnectionDidDieNotification // expected-error {{'NSConnectionDidDieNotification' is unavailable: you may be able to use XPC instead}}
  let cc = NSConnectionReplyMode // expected-error {{'NSConnectionReplyMode' is unavailable: you may be able to use XPC instead}}
  o.classForPortCoder // expected-error {{'classForPortCoder' is unavailable: you may be able to use XPC instead}}
}

func test_NSCalendarDate(o: NSCalendarDate) {} // expected-error {{'NSCalendarDate' is unavailable: use NSCalendar, NSDateComponents, and NSDateFormatter instead}}

func test_dispatch(object: dispatch_object_t) {
  dispatch_retain(object);  // expected-error {{'dispatch_retain' is unavailable}}
  dispatch_release(object); // expected-error {{'dispatch_release' is unavailable}}
}
