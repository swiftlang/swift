// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/user-module -typecheck %s -verify

// REQUIRES: objc_interop

// Type checker should not report any errors in the code below.

import Foundation
import user

var ui: UInt = 5
var i: Int = 56
var p: UnsafeMutablePointer<NSFastEnumerationState>?
var pp: AutoreleasingUnsafeMutablePointer<AnyObject?>?

var userTypedObj = NSUIntTest()

// Check that the NSUInteger comes across as UInt from user Obj C modules.
var ur: UInt = userTypedObj.myCustomMethodThatOperates(onNSUIntegers: ui)

userTypedObj.intProp = ui
userTypedObj.typedefProp = ui
ur = testFunction(ui)
testFunctionInsideMacro(ui)

// Test that nesting works.
var pui: UnsafeMutablePointer<UInt>
ur = testFunctionWithPointerParam(pui)

// NSUIntTest is a user class that conforms to a system defined protocol.

// The types are treated as system types when working with objects having
// protocol type.
var a: [NSFastEnumeration] = [NSUIntTest(), NSUIntTest()]
var r: Int = a[0].countByEnumerating(with: p!, objects: pp!, count: i)

// When working with instances typed as user-defined, NSUInteger comes
// across as UInt.
var rr: UInt = userTypedObj.countByEnumerating(with: p!, objects: pp!, count: ui)

// Check exercising protocol conformance.
func gen<T:NSFastEnumeration>(_ t:T) {
  let i: Int = 56
  let p: UnsafeMutablePointer<NSFastEnumerationState>?
  let pp: AutoreleasingUnsafeMutablePointer<AnyObject?>?
  t.countByEnumerating(with: p!, objects: pp!, count: i)
}

gen(userTypedObj)
