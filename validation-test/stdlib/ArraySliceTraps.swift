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

var ArraySliceTraps = TestSuite("ArraySliceTraps" + testSuiteSuffix)
var ArraySliceLowerBoundOutOfRangeErrorMsg = (
    "Out of bounds: range begins before startIndex"
)

var ArraySliceBoundsErrorMsg = (
    "Out of bounds: index >= endIndex"
)


ArraySliceTraps.test("bounds1/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArraySliceBoundsErrorMsg : "")
  .code {
  var a: ArraySlice<Int> = []
  expectCrashLater()
let x = a[0]
}

ArraySliceTraps.test("bounds2/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArraySliceBoundsErrorMsg : "")
  .code {
  var a: ArraySlice<Int> = []
  expectCrashLater()
let x = a[100]
}

ArraySliceTraps.test("bounds3/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArraySliceBoundsErrorMsg : "")
  .code {
  var a: ArraySlice = [ 10, 20, 30 ]
  expectCrashLater()
let x = a[3]
}

ArraySliceTraps.test("sliceBounds0/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArraySliceLowerBoundOutOfRangeErrorMsg : "")
  .code {
  var a: ArraySlice<Int> = []
  expectCrashLater()
let x = a[-1..<1]
}

ArraySliceTraps.test("sliceBounds1/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArraySliceLowerBoundOutOfRangeErrorMsg : "")
  .code {
  var a: ArraySlice = [ 1 ]
  expectCrashLater()
let x = a[-1..<1]
}

ArraySliceTraps.test("sliceBounds2/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Out of bounds: range begins after bounds.upperBound" : "")
  .code {
  var a: ArraySlice = [ 1 ]
  expectCrashLater()
let x = a[0..<2]
}

ArraySliceTraps.test("sliceBounds3/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Out of bounds: range begins after bounds.upperBound" : "")
  .code {
  var a: ArraySlice = [ 1 ]
  expectCrashLater()
let x = a[1..<2]
}


ArraySliceTraps.test("bounds1/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArraySliceBoundsErrorMsg : "")
  .code {
  var a: ArraySlice<Int> = []
  expectCrashLater()
a[0] = 1
}

ArraySliceTraps.test("bounds2/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArraySliceBoundsErrorMsg : "")
  .code {
  var a: ArraySlice<Int> = []
  expectCrashLater()
a[100] = 1
}

ArraySliceTraps.test("bounds3/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArraySliceBoundsErrorMsg : "")
  .code {
  var a: ArraySlice = [ 10, 20, 30 ]
  expectCrashLater()
a[3] = 1
}

ArraySliceTraps.test("sliceBounds0/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArraySliceLowerBoundOutOfRangeErrorMsg : "")
  .code {
  var a: ArraySlice<Int> = []
  expectCrashLater()
a[-1..<1] = []
}

ArraySliceTraps.test("sliceBounds1/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArraySliceLowerBoundOutOfRangeErrorMsg : "")
  .code {
  var a: ArraySlice = [ 1 ]
  expectCrashLater()
a[-1..<1] = []
}

ArraySliceTraps.test("sliceBounds2/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Out of bounds: range begins after bounds.upperBound" : "")
  .code {
  var a: ArraySlice = [ 1 ]
  expectCrashLater()
a[0..<2] = []
}

ArraySliceTraps.test("sliceBounds3/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Out of bounds: range begins after bounds.upperBound" : "")
  .code {
  var a: ArraySlice = [ 1 ]
  expectCrashLater()
a[1..<2] = []
}


ArraySliceTraps.test("PopFromEmpty")
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Can't remove last element from an empty collection" : "")
  .code {
  var a: ArraySlice<Int> = []
  expectCrashLater()
  a.removeLast()
}

ArraySliceTraps.test("insert(_:at:)/-1")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Negative ArraySlice index is out of range"
    : "")
  .code {
  var a: ArraySlice<Int> = [42]
  expectCrashLater()
  a.insert(3, at: -1)
}
ArraySliceTraps.test("insert(_:at:)/2")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "ArraySlice index is out of range"
    : "")
  .code {
  var a: ArraySlice<Int> = [42]
  expectCrashLater()
  a.insert(3, at: 2)
}

ArraySliceTraps.test("remove(at:)/-1")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArraySliceBoundsErrorMsg : "")
  .code {
  var a: ArraySlice<Int> = [42]
  expectCrashLater()
  a.remove(at: -1)
}
ArraySliceTraps.test("remove(at:)/1")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArraySliceBoundsErrorMsg : "")
  .code {
  var a: ArraySlice<Int> = [42]
  expectCrashLater()
  a.remove(at: 1)
}
ArraySliceTraps.test("remove(at:)/2")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ArraySliceBoundsErrorMsg : "")
  .code {
  var a: ArraySlice<Int> = [42]
  expectCrashLater()
  a.remove(at: 2)
}
