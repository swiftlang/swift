// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out_Debug -Onone -swift-version 4.2 && %target-codesign %t/a.out_Debug && %target-run %t/a.out_Debug
// RUN: %target-build-swift %s -o %t/a.out_Release -O -swift-version 4.2 && %target-codesign %t/a.out_Release && %target-run %t/a.out_Release
// REQUIRES: executable_test

import StdlibUnittest

let testSuiteSuffix = _isDebugAssertConfiguration() ? "_debug" : "_release"

let ContiguousArrayTraps = TestSuite("ContiguousArrayTraps" + testSuiteSuffix)
let ContiguousArrayLowerBoundOutOfRangeErrorMsg = "Out of bounds: range begins before startIndex"
let ContiguousArrayBoundsErrorMsg = "Index out of range"

for i in [0,100] {
  ContiguousArrayTraps.test("bounds/readEmpty/\(i)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? ContiguousArrayBoundsErrorMsg : "")
  .code {
    let a: ContiguousArray<Int> = []
    expectCrashLater()
    _ = a[i]
  }
  
  ContiguousArrayTraps.test("bounds/writeEmpty/\(i)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? ContiguousArrayBoundsErrorMsg : "")
  .code {
    var a: ContiguousArray<Int> = []
    expectCrashLater()
    a[i] = 1
  }
}

ContiguousArrayTraps.test("bounds/read/end")
.skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
.crashOutputMatches(_isDebugAssertConfiguration() ? ContiguousArrayBoundsErrorMsg : "")
.code {
  let a: ContiguousArray = [ 10, 20, 30 ]
  expectCrashLater()
  _ = a[3]
}

ContiguousArrayTraps.test("bounds/write/end")
.skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
.crashOutputMatches(_isDebugAssertConfiguration() ? ContiguousArrayBoundsErrorMsg : "")
.code {
  var a: ContiguousArray = [ 10, 20, 30 ]
  expectCrashLater()
  a[3] = 1
}

for r in [-1..<1,0..<2,1..<2] {
  let crashText = _isDebugAssertConfiguration() 
    ? (r.lowerBound < 0 ? "Negative " : "") + "ContiguousArray index is out of range"
    : ""
  
  ContiguousArrayTraps.test("sliceBounds/empty/read/\(r)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(crashText)
  .code {
    let a: ContiguousArray<Int> = []
    expectCrashLater()
    _ = a[-1..<1]
  }

  ContiguousArrayTraps.test("sliceBounds/empty/write/\(r)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(crashText)
  .code {
    var a: ContiguousArray<Int> = []
    expectCrashLater()
    a[-1..<1] = []
  }

  ContiguousArrayTraps.test("sliceBounds/single/read/\(r)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(crashText)
  .code {
    let a: ContiguousArray = [ 1 ]
    expectCrashLater()
    _ = a[r]
  }

  ContiguousArrayTraps.test("sliceBounds/single/write/\(r)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(crashText)
  .code {
    var a: ContiguousArray = [ 1 ]
    expectCrashLater()
    a[r] = []
  }
}

ContiguousArrayTraps.test("RemoveLastFromEmpty")
.crashOutputMatches(_isDebugAssertConfiguration() ? "Can't remove last element from an empty collection" : "")
.code {
  var a: ContiguousArray<Int> = []
  expectCrashLater()
  a.removeLast()
}

for i in [-1,2] {
  let crashText = _isDebugAssertConfiguration() 
    ? (i < 0 ? "Negative " : "") + "ContiguousArray index is out of range"
    : ""

  ContiguousArrayTraps.test("insert(_:at:)/\(i)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(crashText)
  .code {
    var a: ContiguousArray<Int> = [42]
    expectCrashLater()
    a.insert(3, at: i)
  }  
}

for i in [-1,1,2] {
  ContiguousArrayTraps.test("remove(at:)/\(i)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? ContiguousArrayBoundsErrorMsg : "")
  .code {
    var a: ContiguousArray<Int> = [42]
    expectCrashLater()
    a.remove(at: i)
  }
}

runAllTests()
