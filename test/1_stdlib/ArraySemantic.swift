// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %S/../../utils/line-directive %s -- %target-build-swift %s -o %t/ArraySemantic -Xfrontend -disable-access-control
// RUN: %S/../../utils/line-directive %s -- %target-run %t/ArraySemantic 2>&1 | FileCheck %s

// XFAIL: linux

import StdlibUnittest
import Foundation

var ArraySemanticOptzns = TestSuite("ArraySemanticOptzns")

class ElementClass {
  var val : String
  init(_ x: String) {
    val = x
  }
}

class ViolateInoutSafeySwitchToObjcBuffer {
  final var anArray : [ElementClass] = []

  let nsArray = NSArray(objects: ElementClass("a"), ElementClass("b"),
                        ElementClass("c"))

  @inline(never)
  func accessArrayViaInoutVolation() {
    anArray = _convertNSArrayToArray(nsArray)
  }

  @inline(never)
  func runLoop(inout A : [ElementClass]) {
    // Simulate what happens if we hoist array properties out of a loop and the
    // loop calls a function that violates inout safety and overrides the array.
    let isNative = A._getArrayPropertyIsNative()
    let needsTypeCheck = A._getArrayPropertyNeedsElementTypeCheck()
    for i in 0 ..< A.count {
      A._checkSubscript(i, hoistedIsNativeBuffer: isNative)
      A._getElement(i, hoistedIsNativeBuffer: isNative,
                   hoistedNeedsElementTypeCheck: needsTypeCheck)
      accessArrayViaInoutVolation()
    }
  }

  @inline(never)
  func inoutViolation() {
    anArray = [ ElementClass("1"), ElementClass("2"), ElementClass("3") ]
    runLoop(&anArray)
  }
}

// CHECK-LABEL: RUN {{.*}}ArraySemanticOptzns.inout_rule_violated_isNativeBuffer
// CHECK: fatal error: inout rules were violated: the array was overwritten by an NSArray
ArraySemanticOptzns.test("inout_rule_violated_isNativeBuffer") {
  let v = ViolateInoutSafeySwitchToObjcBuffer()
  expectCrashLater()
  v.inoutViolation()
}

class ViolateInoutSafeyNeedElementTypeCheck {
  final var anArray : [ElementClass] = []

  let nsArray = NSArray(objects: ElementClass("a"), ElementClass("b"),
                        ElementClass("c"))

  @inline(never)
  func accessArrayViaInoutVolation() {
    // Overwrite the array with one that needs an element type check.
    anArray = _convertNSArrayToArray(nsArray)
  }

  @inline(never)
  func runLoop(inout A : [ElementClass]) {
    // Simulate what happens if we hoist array properties out of a loop and the
    // loop calls a function that violates inout safety and overrides the array.
    let needsTypeCheck = A._getArrayPropertyNeedsElementTypeCheck()
    for i in 0 ..< A.count {
      let isNative = A._getArrayPropertyIsNative()
      A._checkSubscript(i, hoistedIsNativeBuffer: isNative)
      A._getElement(i, hoistedIsNativeBuffer: isNative,
                   hoistedNeedsElementTypeCheck: needsTypeCheck)
      accessArrayViaInoutVolation()
    }
  }

  @inline(never)
  func inoutViolation() {
    anArray = [ ElementClass("1"), ElementClass("2"), ElementClass("3")]
    runLoop(&anArray)
  }
}

// CHECK-LABEL: RUN{{.*}}ArraySemanticOptzns.inout_rule_violated_needsElementTypeCheck
// CHECK: fatal error: inout rules were violated: the array was overwritten by an NSArray
ArraySemanticOptzns.test("inout_rule_violated_needsElementTypeCheck") {
  let v = ViolateInoutSafeyNeedElementTypeCheck()
  expectCrashLater()
  v.inoutViolation()
}

runAllTests()
