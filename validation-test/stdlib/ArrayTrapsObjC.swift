// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out_Debug -Onone -swift-version 4.2 && %target-codesign %t/a.out_Debug && %target-run %t/a.out_Debug
// RUN: %target-build-swift %s -o %t/a.out_Release -O -swift-version 4.2 && %target-codesign %t/a.out_Release && %target-run %t/a.out_Release
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

let testSuiteSuffix = _isDebugAssertConfiguration() ? "_debug" : "_release"

var ArrayTraps = TestSuite("ArrayTraps" + testSuiteSuffix)

class Base { }
class Derived : Base { }
class Derived2 : Derived { }

ArrayTraps.test("downcast1")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let ba: [Base] = [ Derived(), Base() ]
  let da = ba as! [Derived]
  _ = da[0]
  expectCrashLater()
  _ = da[1]
}

ArrayTraps.test("downcast2")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let a: [AnyObject] = ["String" as NSString, 1 as NSNumber]
  let sa = a as! [NSString]
  _ = sa[0]
  expectCrashLater()
  _ = sa[1]
}

ArrayTraps.test("downcast3")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let ba: [Base] = [ Derived2(), Derived(), Base() ]
  let d2a = ba as! [Derived2]
  _ = d2a[0]
  let d1a = d2a as [Derived]
  _ = d1a[0]
  _ = d1a[1]
  expectCrashLater()
  _ = d1a[2]
}

@objc protocol ObjCProto { }
class ObjCBase : NSObject, ObjCProto { }
class ObjCDerived : ObjCBase { }

ArrayTraps.test("downcast4")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let ba: [ObjCProto] = [ ObjCDerived(), ObjCBase() ]
  let da = ba as! [ObjCDerived]
  _ = da[0]
  expectCrashLater()
  _ = da[1]
}

ArrayTraps.test("bounds_with_downcast")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Fatal error: Index out of range" : "")
  .code {
  let ba: [Base] = [ Derived(), Base() ]
  let da = ba as! [Derived]
  expectCrashLater()
  _ = da[2]
}

var ArraySemanticOptzns = TestSuite("ArraySemanticOptzns" + testSuiteSuffix)

class BaseClass {
}

class ElementClass : BaseClass {
  var val: String
  init(_ x: String) {
    val = x
  }
}

class ViolateInoutSafetySwitchToObjcBuffer {
  final var anArray: [ElementClass] = []

  let nsArray = NSArray(
    objects: ElementClass("a"), ElementClass("b"), ElementClass("c"))

  @inline(never)
  func accessArrayViaInoutViolation() {
    anArray = nsArray as! [ElementClass]
  }

  @inline(never)
  func runLoop(_ A: inout [ElementClass]) {
    // Simulate what happens if we hoist array properties out of a loop and the
    // loop calls a function that violates inout safety and overrides the array.
    let isNativeTypeChecked = A._hoistableIsNativeTypeChecked()
    for i in 0..<A.count {
      let t = A._checkSubscript(
        i, wasNativeTypeChecked: isNativeTypeChecked)
      _ = A._getElement(
        i, wasNativeTypeChecked: isNativeTypeChecked, matchingSubscriptCheck: t)
      accessArrayViaInoutViolation()
    }
  }

  @inline(never)
  func inoutViolation() {
    anArray = [ ElementClass("1"), ElementClass("2"), ElementClass("3") ]
    runLoop(&anArray)
  }
}

ArraySemanticOptzns.test("inout_rule_violated_isNativeBuffer")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Fatal access conflict detected." : "")
  .code {
  let v = ViolateInoutSafetySwitchToObjcBuffer()
  expectCrashLater()
  v.inoutViolation()
}

class ViolateInoutSafetyNeedElementTypeCheck {
  final var anArray : [ElementClass] = []

  @inline(never)
  func accessArrayViaInoutViolation() {
    // Overwrite the array with one that needs an element type check.
    let ba: [BaseClass] = [ BaseClass(), BaseClass() ]
    anArray = ba as! [ElementClass]
  }

  @inline(never)
  func runLoop(_ A: inout [ElementClass]) {
    // Simulate what happens if we hoist array properties out of a loop and the
    // loop calls a function that violates inout safety and overrides the array.
    let isNativeTypeChecked = A._hoistableIsNativeTypeChecked()
    for i in 0..<A.count {
      let t = A._checkSubscript(
        i, wasNativeTypeChecked: isNativeTypeChecked)
      _ = A._getElement(
        i, wasNativeTypeChecked: isNativeTypeChecked, matchingSubscriptCheck: t)
      accessArrayViaInoutViolation()
    }
  }

  @inline(never)
  func inoutViolation() {
    anArray = [ ElementClass("1"), ElementClass("2"), ElementClass("3")]
    runLoop(&anArray)
  }
}

ArraySemanticOptzns.test("inout_rule_violated_needsElementTypeCheck")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Fatal access conflict detected." : "")
  .code {
  let v = ViolateInoutSafetyNeedElementTypeCheck()
  expectCrashLater()
  v.inoutViolation()
}

runAllTests()
