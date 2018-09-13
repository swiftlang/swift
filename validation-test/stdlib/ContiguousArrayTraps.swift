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

var ContiguousArrayTraps = TestSuite("ContiguousArrayTraps" + testSuiteSuffix)
var ContiguousArrayLowerBoundOutOfRangeErrorMsg = (
    "Out of bounds: range begins before startIndex"
)

var ContiguousArrayBoundsErrorMsg = (
    "Index out of range"
)


ContiguousArrayTraps.test("bounds1/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ContiguousArrayBoundsErrorMsg : "")
  .code {
  var a: ContiguousArray<Int> = []
  expectCrashLater()
let x = a[0]
}

ContiguousArrayTraps.test("bounds2/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ContiguousArrayBoundsErrorMsg : "")
  .code {
  var a: ContiguousArray<Int> = []
  expectCrashLater()
let x = a[100]
}

ContiguousArrayTraps.test("bounds3/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ContiguousArrayBoundsErrorMsg : "")
  .code {
  var a: ContiguousArray = [ 10, 20, 30 ]
  expectCrashLater()
let x = a[3]
}

ContiguousArrayTraps.test("sliceBounds0/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ContiguousArrayLowerBoundOutOfRangeErrorMsg : "")
  .code {
  var a: ContiguousArray<Int> = []
  expectCrashLater()
let x = a[-1..<1]
}

ContiguousArrayTraps.test("sliceBounds1/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ContiguousArrayLowerBoundOutOfRangeErrorMsg : "")
  .code {
  var a: ContiguousArray = [ 1 ]
  expectCrashLater()
let x = a[-1..<1]
}

ContiguousArrayTraps.test("sliceBounds2/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Out of bounds: range begins after bounds.upperBound" : "")
  .code {
  var a: ContiguousArray = [ 1 ]
  expectCrashLater()
let x = a[0..<2]
}

ContiguousArrayTraps.test("sliceBounds3/read")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Out of bounds: range begins after bounds.upperBound" : "")
  .code {
  var a: ContiguousArray = [ 1 ]
  expectCrashLater()
let x = a[1..<2]
}


ContiguousArrayTraps.test("bounds1/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ContiguousArrayBoundsErrorMsg : "")
  .code {
  var a: ContiguousArray<Int> = []
  expectCrashLater()
a[0] = 1
}

ContiguousArrayTraps.test("bounds2/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ContiguousArrayBoundsErrorMsg : "")
  .code {
  var a: ContiguousArray<Int> = []
  expectCrashLater()
a[100] = 1
}

ContiguousArrayTraps.test("bounds3/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ContiguousArrayBoundsErrorMsg : "")
  .code {
  var a: ContiguousArray = [ 10, 20, 30 ]
  expectCrashLater()
a[3] = 1
}

ContiguousArrayTraps.test("sliceBounds0/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ContiguousArrayLowerBoundOutOfRangeErrorMsg : "")
  .code {
  var a: ContiguousArray<Int> = []
  expectCrashLater()
a[-1..<1] = []
}

ContiguousArrayTraps.test("sliceBounds1/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ContiguousArrayLowerBoundOutOfRangeErrorMsg : "")
  .code {
  var a: ContiguousArray = [ 1 ]
  expectCrashLater()
a[-1..<1] = []
}

ContiguousArrayTraps.test("sliceBounds2/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Out of bounds: range begins after bounds.upperBound" : "")
  .code {
  var a: ContiguousArray = [ 1 ]
  expectCrashLater()
a[0..<2] = []
}

ContiguousArrayTraps.test("sliceBounds3/write")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Out of bounds: range begins after bounds.upperBound" : "")
  .code {
  var a: ContiguousArray = [ 1 ]
  expectCrashLater()
a[1..<2] = []
}


ContiguousArrayTraps.test("PopFromEmpty")
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Can't remove last element from an empty collection" : "")
  .code {
  var a: ContiguousArray<Int> = []
  expectCrashLater()
  a.removeLast()
}

ContiguousArrayTraps.test("insert(_:at:)/-1")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "Negative ContiguousArray index is out of range"
    : "")
  .code {
  var a: ContiguousArray<Int> = [42]
  expectCrashLater()
  a.insert(3, at: -1)
}
ContiguousArrayTraps.test("insert(_:at:)/2")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "ContiguousArray index is out of range"
    : "")
  .code {
  var a: ContiguousArray<Int> = [42]
  expectCrashLater()
  a.insert(3, at: 2)
}

ContiguousArrayTraps.test("remove(at:)/-1")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ContiguousArrayBoundsErrorMsg : "")
  .code {
  var a: ContiguousArray<Int> = [42]
  expectCrashLater()
  a.remove(at: -1)
}
ContiguousArrayTraps.test("remove(at:)/1")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ContiguousArrayBoundsErrorMsg : "")
  .code {
  var a: ContiguousArray<Int> = [42]
  expectCrashLater()
  a.remove(at: 1)
}
ContiguousArrayTraps.test("remove(at:)/2")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    ContiguousArrayBoundsErrorMsg : "")
  .code {
  var a: ContiguousArray<Int> = [42]
  expectCrashLater()
  a.remove(at: 2)
}
