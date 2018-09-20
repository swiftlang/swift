// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out_Debug -Onone -swift-version 4.2 && %target-codesign %t/a.out_Debug && %target-run %t/a.out_Debug
// RUN: %target-build-swift %s -o %t/a.out_Release -O -swift-version 4.2 && %target-codesign %t/a.out_Release && %target-run %t/a.out_Release
// REQUIRES: executable_test

import StdlibUnittest

let testSuiteSuffix = _isDebugAssertConfiguration() ? "_debug" : "_release"

let ArrayTraps = TestSuite("ArrayTraps" + testSuiteSuffix)
let ArrayLowerBoundOutOfRangeErrorMsg = "Out of bounds: range begins before startIndex"
let ArrayBoundsErrorMsg = "Index out of range"

for i in [0,100] {
  ArrayTraps.test("bounds/readEmpty/\(i)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? ArrayBoundsErrorMsg : "")
  .code {
    let a: Array<Int> = []
    expectCrashLater()
    _ = a[i]
  }
  
  ArrayTraps.test("bounds/writeEmpty/\(i)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? ArrayBoundsErrorMsg : "")
  .code {
    var a: Array<Int> = []
    expectCrashLater()
    a[i] = 1
  }
}

ArrayTraps.test("bounds/read/end")
.skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
.crashOutputMatches(_isDebugAssertConfiguration() ? ArrayBoundsErrorMsg : "")
.code {
  let a: Array = [ 10, 20, 30 ]
  expectCrashLater()
  _ = a[3]
}

ArrayTraps.test("bounds/write/end")
.skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
.crashOutputMatches(_isDebugAssertConfiguration() ? ArrayBoundsErrorMsg : "")
.code {
  var a: Array = [ 10, 20, 30 ]
  expectCrashLater()
  a[3] = 1
}

for r in [-1..<1,0..<2,1..<2] {
  let crashText = _isDebugAssertConfiguration() 
    ? (r.lowerBound < 0 ? "Negative " : "") + "Array index is out of range"
    : ""
  
  ArrayTraps.test("sliceBounds/empty/read/\(r)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(crashText)
  .code {
    let a: Array<Int> = []
    expectCrashLater()
    _ = a[-1..<1]
  }

  ArrayTraps.test("sliceBounds/empty/write/\(r)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(crashText)
  .code {
    var a: Array<Int> = []
    expectCrashLater()
    a[-1..<1] = []
  }

  ArrayTraps.test("sliceBounds/single/read/\(r)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(crashText)
  .code {
    let a: Array = [ 1 ]
    expectCrashLater()
    _ = a[r]
  }

  ArrayTraps.test("sliceBounds/single/write/\(r)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(crashText)
  .code {
    var a: Array = [ 1 ]
    expectCrashLater()
    a[r] = []
  }
}

ArrayTraps.test("RemoveLastFromEmpty")
.crashOutputMatches(_isDebugAssertConfiguration() ? "Can't remove last element from an empty collection" : "")
.code {
  var a: Array<Int> = []
  expectCrashLater()
  a.removeLast()
}

for i in [-1,2] {
  let crashText = _isDebugAssertConfiguration() 
    ? (i < 0 ? "Negative " : "") + "Array index is out of range"
    : ""

  ArrayTraps.test("insert(_:at:)/\(i)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(crashText)
  .code {
    var a: Array<Int> = [42]
    expectCrashLater()
    a.insert(3, at: i)
  }  
}

for i in [-1,1,2] {
  ArrayTraps.test("remove(at:)/\(i)")
  .skip(.custom(_isFastAssertConfiguration, reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ? ArrayBoundsErrorMsg : "")
  .code {
    var a: Array<Int> = [42]
    expectCrashLater()
    a.remove(at: i)
  }
}

runAllTests()
