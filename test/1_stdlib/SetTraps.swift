// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out_Debug
// RUN: %target-build-swift %s -o %t/a.out_Release -O
//
// RUN: %target-run %t/a.out_Debug
// RUN: %target-run %t/a.out_Release
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

var SetTraps = TestSuite("SetTraps")

SetTraps.test("RemoveInvalidIndex1")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = Set<Int>()
  let index = s.startIndex
  expectCrashLater()
  s.removeAtIndex(index)
}

SetTraps.test("RemoveInvalidIndex2")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = Set<Int>()
  let index = s.endIndex
  expectCrashLater()
  s.removeAtIndex(index)
}

SetTraps.test("RemoveInvalidIndex3")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s: Set<Int> = [ 10, 20, 30 ]
  let index = s.endIndex
  expectCrashLater()
  s.removeAtIndex(index)
}

SetTraps.test("RemoveInvalidIndex4")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s: Set<Int> = [ 10 ]
  let index = s.indexOf(10)!
  s.removeAtIndex(index)
  expectFalse(s.contains(10))
  expectCrashLater()
  s.removeAtIndex(index)
}

SetTraps.test("RemoveFirstFromEmpty")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "can't removeFirst from an empty Set" : "")
  .code {
  var s = Set<Int>()
  expectCrashLater()
  s.removeFirst()
}

runAllTests()

