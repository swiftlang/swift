// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out_Debug
// RUN: %target-build-swift %s -o %t/a.out_Release -O
//
// RUN: %target-run %t/a.out_Debug
// RUN: %target-run %t/a.out_Release
// REQUIRES: executable_test

import StdlibUnittest

let testSuiteSuffix = _isDebugAssertConfiguration() ? "_debug" : "_release"

var StringTraps = TestSuite("StringTraps" + testSuiteSuffix)

StringTraps.test("startIndex/predecessor")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.startIndex
  i = s.index(after: i)
  i = s.index(before: i)
  expectCrashLater()
  i = s.index(before: i)
}

StringTraps.test("endIndex/successor")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.startIndex
  i = s.index(after: i)
  i = s.index(after: i)
  i = s.index(after: i)
  expectCrashLater()
  i = s.index(after: i)
}

StringTraps.test("subscript(_:)/endIndex")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.startIndex
  i = s.index(after: i)
  i = s.index(after: i)
  i = s.index(after: i)
  expectCrashLater()
  s[i]
}

StringTraps.test("UTF8ViewEndIndexSuccessor")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.utf8.startIndex
  i = s.utf8.index(after: i)
  i = s.utf8.index(after: i)
  i = s.utf8.index(after: i)
  expectCrashLater()
  i = s.utf8.index(after: i)
}

StringTraps.test("UTF8ViewSubscript/endIndex")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.utf8.startIndex
  i = s.utf8.index(after: i)
  i = s.utf8.index(after: i)
  i = s.utf8.index(after: i)
  expectCrashLater()
  s.utf8[i]
}

StringTraps.test("UTF16ViewSubscript/DecrementedStartIndex")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.utf16.startIndex
  i = s.utf16.index(before: i)
  expectCrashLater()
  s.utf16[i]
}

StringTraps.test("UTF16ViewSubscript/endIndex")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var s = "abc"
  var i = s.utf16.startIndex
  i = s.utf16.index(after: i)
  i = s.utf16.index(after: i)
  i = s.utf16.index(after: i)
  expectCrashLater()
  s.utf16[i]
}

runAllTests()

