// Also run this test in optimize test modes.
// REQUIRES: optimize_test

// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out_Debug
// RUN: %target-build-swift %s -o %t/a.out_Release -O
//
// RUN: %target-run %t/a.out_Debug
// RUN: %target-run %t/a.out_Release

// XFAIL: linux

import StdlibUnittest
import Foundation

var StringTraps = TestSuite("StringTraps")

StringTraps.test("startIndex/predecessor")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.startIndex
  ++i
  --i
  expectCrashLater()
  --i
}

StringTraps.test("endIndex/successor")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.startIndex
  ++i
  ++i
  ++i
  expectCrashLater()
  ++i
}

StringTraps.test("subscript(_:)/endIndex")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.startIndex
  ++i
  ++i
  ++i
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
  ++i
  ++i
  ++i
  expectCrashLater()
  ++i
}

StringTraps.test("UTF8ViewSubscript/endIndex")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.utf8.startIndex
  ++i
  ++i
  ++i
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
  --i
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
  ++i
  ++i
  ++i
  expectCrashLater()
  s.utf16[i]
}

runAllTests()

