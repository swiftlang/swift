// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -I %S/Inputs/custom-modules %s -verify-ignore-unknown

// REQUIRES: objc_interop

import Dispatch
import Foundation
import stdio
import AvailabilityExtras

// Test if an instance method marked __attribute__((unavailable)) on
// the *class* NSObject can be used.
func test_unavailable_instance_method(_ x : NSObject) -> Bool {
  return x.allowsWeakReference() // expected-error {{'allowsWeakReference()' is unavailable}}
}

func test_unavailable_method_in_protocol(_ x : NSObjectProtocol) {
  // expected-warning @+1 {{expression of type 'NSObjectProtocol' is unused}}
  x.retain() // expected-error {{'retain()' is unavailable}}
}
func test_unavailable_method_in_protocol_use_class_instance(_ x : NSObject) {
  x.retain() // expected-error {{'retain()' is unavailable}} expected-warning {{result of call to 'retain()' is unused}}
}

func test_unavailable_func(_ x : NSObject) {
  NSDeallocateObject(x) // expected-error {{'NSDeallocateObject' is unavailable}}
}

func test_deprecated_imported_as_unavailable(_ s:UnsafeMutablePointer<CChar>) {
  _ = tmpnam(s) // expected-warning {{'tmpnam' is deprecated: Due to security concerns inherent in the design of tmpnam(3), it is highly recommended that you use mkstemp(3) instead.}}
}

func test_NSInvocation(_ x: NSInvocation,         // expected-error {{'NSInvocation' is unavailable}}
                       y: NSInvocationOperation,// expected-error {{'NSInvocationOperation' is unavailable}}
                       z: NSMethodSignature) {} // expected-error {{'NSMethodSignature' is unavailable}}

func test_class_avail(_ x:NSObject, obj: AnyObject) {
  x.`class`() // expected-error {{'class()' is unavailable in Swift: use 'type(of:)' instead}} expected-warning {{result of call to 'class()' is unused}}
  _ = NSObject.`class`() // expected-error {{'class()' is unavailable in Swift: use 'self' instead}}
  _ = obj.`class`!() // expected-error {{'class()' is unavailable in Swift: use 'type(of:)' instead}}
}

func test_unavailable_app_extension() {
  // No expected error here.  See corresponding App extension test.
  _ = SomeCrazyAppExtensionForbiddenAPI() // no-error
}

func test_swift_unavailable() {
  NSSwiftOldUnavailableFunction() // expected-error {{'NSSwiftOldUnavailableFunction()' is unavailable in Swift}}
  NSSwiftNewUnavailableFunction() // expected-error {{'NSSwiftNewUnavailableFunction()' is unavailable in Swift}}
  NSSwiftNewUnavailableFunction2() // expected-error {{'NSSwiftNewUnavailableFunction2()' is unavailable in Swift}}
  NSSwiftNewUnavailableFunctionPremium() // expected-error {{'NSSwiftNewUnavailableFunctionPremium()' is unavailable in Swift: You didn't want to use it anyway.}}

  let x: NSSwiftUnavailableStruct? = nil // expected-error {{'NSSwiftUnavailableStruct' is unavailable in Swift}}
}

func test_CFReleaseRetainAutorelease(_ x: CFTypeRef, color: CGColor) {
  CFRelease(x)              // expected-error {{'CFRelease' is unavailable: Core Foundation objects are automatically memory managed}}
  CGColorRelease(color)     // expected-error {{'CGColorRelease' is unavailable: Core Foundation objects are automatically memory managed}}
  CFRetain(x)               // expected-error {{'CFRetain' is unavailable: Core Foundation objects are automatically memory managed}} expected-warning {{result of call to 'CFRetain' is unused}}
  CGColorRetain(color)      // expected-error {{'CGColorRetain' is unavailable: Core Foundation objects are automatically memory managed}} expected-warning {{result of call to 'CGColorRetain' is unused}}
  CFAutorelease(x)          // expected-error {{'CFAutorelease' is unavailable: Core Foundation objects are automatically memory managed}} expected-warning {{result of call to 'CFAutorelease' is unused}}
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

func test_NSZone(_ z : NSZone) { 
  NSCreateZone(1, 1, true)  // expected-error {{'NSCreateZone' is unavailable}} expected-warning {{result of call to 'NSCreateZone' is unused}}
  NSSetZoneName(z, "name")  // expected-error {{'NSSetZoneName' is unavailable}}
  NSZoneName(z)             // expected-error {{'NSZoneName' is unavailable}} expected-warning {{result of call to 'NSZoneName' is unused}}
}

func test_DistributedObjects(_ o: NSObject,
                             a: NSConnection,           // expected-error {{'NSConnection' is unavailable in Swift: Use NSXPCConnection instead}}
                             b: NSConnectionDelegate,   // expected-error {{'NSConnectionDelegate' is unavailable in Swift: Use NSXPCConnection instead}}
                             c: NSDistantObjectRequest, // expected-error {{'NSDistantObjectRequest' is unavailable in Swift: Use NSXPCConnection instead}}
                             d: NSDistantObject,        // expected-error {{'NSDistantObject' is unavailable in Swift: Use NSXPCConnection instead}}
                             e: NSPortNameServer,       // expected-error {{'NSPortNameServer' is unavailable in Swift: Use NSXPCConnection instead}}
                             f: NSMachBootstrapServer,  // expected-error {{'NSMachBootstrapServer' is unavailable in Swift: Use NSXPCConnection instead}}
                             g: NSMessagePortNameServer, // expected-error {{'NSMessagePortNameServer' is unavailable in Swift: Use NSXPCConnection instead}}
                             h: NSSocketPortNameServer, // expected-error {{'NSSocketPortNameServer' is unavailable in Swift: Use NSXPCConnection instead}}
                             i: NSPortCoder) {          // expected-error {{'NSPortCoder' is unavailable in Swift: Use NSXPCConnection instead}}

  let ca = NSConnectionDidDieNotification // expected-error {{'NSConnectionDidDieNotification' is unavailable in Swift: Use NSXPCConnection instead}}
  let cc = NSConnectionReplyMode // expected-error {{'NSConnectionReplyMode' is unavailable in Swift: Use NSXPCConnection instead}}
  _ = o.classForPortCoder // expected-error {{'classForPortCoder' is unavailable in Swift: Use NSXPCConnection instead}}
}

func test_NSCalendarDate(_ o: NSCalendarDate) {} // expected-error {{'NSCalendarDate' is unavailable in Swift: Use NSCalendar and NSDateComponents and NSDateFormatter instead}}

func testImportAsMember() {
  _ = CGColorCreateGenericGray(0.5, 1.0) // expected-error {{'CGColorCreateGenericGray' has been replaced by 'CGColor.init(gray:alpha:)'}} {{7-31=CGColor}} {{32-32=gray: }} {{37-37=alpha: }}
  _ = CGColor(gray: 0.5, alpha: 1.0)
}

func testUnavailableRenamedEnum() {
  _ = NSClothingStyle.hipster
  _ = NSClothingStyleOfficeCasual // expected-error{{'NSClothingStyleOfficeCasual' has been renamed to 'NSClothingStyle.semiFormal'}} {{7-34=NSClothingStyle.semiFormal}}
}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected warning produced: imported declaration 'dispatch_sync' could not be mapped to 'DispatchQueue.sync(self:execute:)'
// <unknown>:0: error: unexpected note produced: 'NSConnectionDidDieNotification' has been explicitly marked unavailable here
// <unknown>:0: error: unexpected note produced: 'CGColorCreateGenericGray' was obsoleted in Swift 3
