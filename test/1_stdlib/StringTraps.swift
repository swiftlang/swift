// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out_Debug
// RUN: %target-build-swift %s -o %t/a.out_Release -O
//
// RUN: %target-run %t/a.out_Debug
// RUN: %target-run %t/a.out_Release
// REQUIRES: executable_test

// XFAIL: linux

import StdlibUnittest
import Foundation

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

var StringTraps = TestSuite("StringTraps")

StringTraps.test("startIndex/predecessor")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.startIndex
  i = i.successor()
  i = i.predecessor()
  expectCrashLater()
  i = i.predecessor()
}

StringTraps.test("endIndex/successor")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.startIndex
  i = i.successor()
  i = i.successor()
  i = i.successor()
  expectCrashLater()
  i = i.successor()
}

StringTraps.test("subscript(_:)/endIndex")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.startIndex
  i = i.successor()
  i = i.successor()
  i = i.successor()
  expectCrashLater()
  s[i]
}

StringTraps.test("UTF8ViewEndIndexSuccessor")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.utf8.startIndex
  i = i.successor()
  i = i.successor()
  i = i.successor()
  expectCrashLater()
  i = i.successor()
}

StringTraps.test("UTF8ViewSubscript/endIndex")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.utf8.startIndex
  i = i.successor()
  i = i.successor()
  i = i.successor()
  expectCrashLater()
  s.utf8[i]
}

StringTraps.test("UTF16ViewSubscript/DecrementedStartIndex")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.utf16.startIndex
  i = i.predecessor()
  expectCrashLater()
  s.utf16[i]
}

StringTraps.test("UTF16ViewSubscript/endIndex")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.utf16.startIndex
  i = i.successor()
  i = i.successor()
  i = i.successor()
  expectCrashLater()
  s.utf16[i]
}

runAllTests()

