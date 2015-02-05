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

StringTraps.test("startIndex/predecessor") {
  var s = "abc"
  var i = s.startIndex
  ++i
  --i
  expectCrashLater()
  --i
}

StringTraps.test("endIndex/successor") {
  var s = "abc"
  var i = s.startIndex
  ++i
  ++i
  ++i
  expectCrashLater()
  ++i
}

StringTraps.test("subscript(_:)/endIndex") {
  var s = "abc"
  var i = s.startIndex
  ++i
  ++i
  ++i
  expectCrashLater()
  s[i]
}

StringTraps.test("UTF8ViewEndIndexSuccessor") {
  var s = "abc"
  var i = s.utf8.startIndex
  ++i
  ++i
  ++i
  expectCrashLater()
  ++i
}

StringTraps.test("UTF8ViewSubscript/endIndex") {
  var s = "abc"
  var i = s.utf8.startIndex
  ++i
  ++i
  ++i
  expectCrashLater()
  s.utf8[i]
}

StringTraps.test("UTF16ViewSubscript/DecrementedStartIndex") {
  var s = "abc"
  var i = s.utf16.startIndex
  --i
  expectCrashLater()
  s.utf16[i]
}

StringTraps.test("UTF16ViewSubscript/endIndex") {
  var s = "abc"
  var i = s.utf16.startIndex
  ++i
  ++i
  ++i
  expectCrashLater()
  s.utf16[i]
}

runAllTests()

