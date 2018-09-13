// RUN: %empty-directory(%t)
// RUN: %gyb %s -o %t/ArrayTraps.swift
// RUN: %line-directive %t/ArrayTraps.swift -- %target-build-swift %t/ArrayTraps.swift -o %t/a.out_Debug -Onone
// RUN: %line-directive %t/ArrayTraps.swift -- %target-build-swift %t/ArrayTraps.swift -o %t/a.out_Release -O
//
// RUN: %target-codesign %t/a.out_Debug
// RUN: %target-codesign %t/a.out_Release
// RUN: %line-directive %t/ArrayTraps.swift -- %target-run %t/a.out_Debug
// RUN: %line-directive %t/ArrayTraps.swift -- %target-run %t/a.out_Release
// REQUIRES: executable_test

import StdlibUnittest

let testSuiteSuffix = _isDebugAssertConfiguration() ? "_debug" : "_release"

var ArrayTraps = TestSuite("ArrayTraps" + testSuiteSuffix)
var ArrayLowerBoundOutOfRangeErrorMsg = (
    "Out of bounds: range begins before startIndex"
)

var ArrayBoundsErrorMsg = (
    "Index out of range"
)


ArrayTraps.test("bounds1/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArrayBoundsErrorMsg : "")
  .code {
  var a: Array<Int> = []
  expectCrashLater()
let x = a[0]
}

ArrayTraps.test("bounds2/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArrayBoundsErrorMsg : "")
  .code {
  var a: Array<Int> = []
  expectCrashLater()
let x = a[100]
}

ArrayTraps.test("bounds3/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArrayBoundsErrorMsg : "")
  .code {
  var a: Array = [ 10, 20, 30 ]
  expectCrashLater()
let x = a[3]
}

ArrayTraps.test("sliceBounds0/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArrayLowerBoundOutOfRangeErrorMsg : "")
  .code {
  var a: Array<Int> = []
  expectCrashLater()
let x = a[-1..<1]
}

ArrayTraps.test("sliceBounds1/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArrayLowerBoundOutOfRangeErrorMsg : "")
  .code {
  var a: Array = [ 1 ]
  expectCrashLater()
let x = a[-1..<1]
}

ArrayTraps.test("sliceBounds2/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Out of bounds: range begins after bounds.upperBound" : "")
  .code {
  var a: Array = [ 1 ]
  expectCrashLater()
let x = a[0..<2]
}

ArrayTraps.test("sliceBounds3/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Out of bounds: range begins after bounds.upperBound" : "")
  .code {
  var a: Array = [ 1 ]
  expectCrashLater()
let x = a[1..<2]
}


ArrayTraps.test("bounds1/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArrayBoundsErrorMsg : "")
  .code {
  var a: Array<Int> = []
  expectCrashLater()
a[0] = 1
}

ArrayTraps.test("bounds2/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArrayBoundsErrorMsg : "")
  .code {
  var a: Array<Int> = []
  expectCrashLater()
a[100] = 1
}

ArrayTraps.test("bounds3/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArrayBoundsErrorMsg : "")
  .code {
  var a: Array = [ 10, 20, 30 ]
  expectCrashLater()
a[3] = 1
}

ArrayTraps.test("sliceBounds0/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArrayLowerBoundOutOfRangeErrorMsg : "")
  .code {
  var a: Array<Int> = []
  expectCrashLater()
a[-1..<1] = []
}

ArrayTraps.test("sliceBounds1/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArrayLowerBoundOutOfRangeErrorMsg : "")
  .code {
  var a: Array = [ 1 ]
  expectCrashLater()
a[-1..<1] = []
}

ArrayTraps.test("sliceBounds2/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Out of bounds: range begins after bounds.upperBound" : "")
  .code {
  var a: Array = [ 1 ]
  expectCrashLater()
a[0..<2] = []
}

ArrayTraps.test("sliceBounds3/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Out of bounds: range begins after bounds.upperBound" : "")
  .code {
  var a: Array = [ 1 ]
  expectCrashLater()
a[1..<2] = []
}


ArrayTraps.test("PopFromEmpty")
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Can't remove last element from an empty collection" : "")
  .code {
  var a: Array<Int> = []
  expectCrashLater()
  a.removeLast()
}

ArrayTraps.test("insert(_:at:)/-1")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Negative Array index is out of range"
    : "")
  .code {
  var a: Array<Int> = [42]
  expectCrashLater()
  a.insert(3, at: -1)
}
ArrayTraps.test("insert(_:at:)/2")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Array index is out of range"
    : "")
  .code {
  var a: Array<Int> = [42]
  expectCrashLater()
  a.insert(3, at: 2)
}

ArrayTraps.test("remove(at:)/-1")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArrayBoundsErrorMsg : "")
  .code {
  var a: Array<Int> = [42]
  expectCrashLater()
  a.remove(at: -1)
}
ArrayTraps.test("remove(at:)/1")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArrayBoundsErrorMsg : "")
  .code {
  var a: Array<Int> = [42]
  expectCrashLater()
  a.remove(at: 1)
}
ArrayTraps.test("remove(at:)/2")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArrayBoundsErrorMsg : "")
  .code {
  var a: Array<Int> = [42]
  expectCrashLater()
  a.remove(at: 2)
}
