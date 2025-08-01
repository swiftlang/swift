// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out_Debug -Onone
// RUN: %target-build-swift %s -o %t/a.out_Release -O
//
// RUN: %target-codesign %t/a.out_Debug
// RUN: %target-codesign %t/a.out_Release
// RUN: env %env-SWIFT_BINARY_COMPATIBILITY_VERSION=0x050700 %target-run %t/a.out_Debug
// RUN: env %env-SWIFT_BINARY_COMPATIBILITY_VERSION=0x050700 %target-run %t/a.out_Release

// Note: the environment variable above forces the stdlib's bincompat version to
// 5.7 so that we can test new behavior even if the SDK we're using predates it.

// REQUIRES: executable_test
// UNSUPPORTED: OS=wasip1

import StdlibUnittest
#if _runtime(_ObjC)
import Foundation // For NSString
#endif

let testSuiteSuffix = _isDebugAssertConfiguration() ? "_debug" : "_release"

var StringTraps = TestSuite("StringTraps" + testSuiteSuffix)
defer { runAllTests() }

StringTraps.test("startIndex/predecessor")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let s = "abc"
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
  let s = "abc"
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
  let s = "abc"
  var i = s.startIndex
  i = s.index(after: i)
  i = s.index(after: i)
  i = s.index(after: i)
  expectCrashLater()
  _ = s[i]
}

StringTraps.test("String.index(before:) trap on i > endIndex")
.skip(
  .custom({ _isFastAssertConfiguration() },
  reason: "trap is not guaranteed to happen in -Ounchecked"))
.code {
  guard #available(SwiftStdlib 5.7, *) else { return }

  let long = String(repeating: "X", count: 1024)
  let short = "This is a short string"
  expectCrashLater()
  let i = short.index(before: long.endIndex)
  print(i)
}

StringTraps.test("String.index(before:) trap on i == startIndex after scalar alignment")
.skip(
  .custom({ _isFastAssertConfiguration() },
  reason: "trap is not guaranteed to happen in -Ounchecked"))
.code {
  guard #available(SwiftStdlib 5.7, *) else { return }

  let s = "🥯 Bagel with schmear"
  let i = s.utf8.index(after: s.utf8.startIndex)
  expectCrashLater()
  // `i` is equivalent to `s.startIndex` as far as `String` is concerned
  let j = s.index(before: i)
  print(j)
}

StringTraps.test("UTF8ViewSubscript/endIndexSuccessor")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let s = "abc"
  var i = s.utf8.startIndex
  i = s.utf8.index(after: i)
  i = s.utf8.index(after: i)
  i = s.utf8.index(after: i)
  expectCrashLater()
  i = s.utf8.index(after: i)
  _ = s.utf8[i]
}

StringTraps.test("UTF8ViewSubscript/endIndex")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let s = "abc"
  var i = s.utf8.startIndex
  i = s.utf8.index(after: i)
  i = s.utf8.index(after: i)
  i = s.utf8.index(after: i)
  expectCrashLater()
  _ = s.utf8[i]
}

StringTraps.test("UTF16ViewSubscript/DecrementedStartIndex")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let s = "abc"
  var i = s.utf16.startIndex
  expectCrashLater()
  i = s.utf16.index(before: i)
  _ = s.utf16[i]
}

StringTraps.test("UTF16ViewSubscript/endIndex")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let s = "abc"
  var i = s.utf16.startIndex
  i = s.utf16.index(after: i)
  i = s.utf16.index(after: i)
  i = s.utf16.index(after: i)
  expectCrashLater()
  _ = s.utf16[i]
}

StringTraps.test("UTF16ViewIndex/offsetLimited")
  .code {
  let sa = "foo"
  let u16a = sa.utf16
  let s16 = sa + "🤦🏻‍♀️"
  let u16 = s16.utf16

  let iaBegin = u16a.index(sa.startIndex, offsetBy: 99, limitedBy: sa.endIndex)
  expectNil(iaBegin)
  let iaEnd = u16a.index(sa.endIndex, offsetBy: 99, limitedBy: sa.endIndex)
  expectNil(iaEnd)
  let i16Begin = u16.index(u16.startIndex, offsetBy: 99, limitedBy: u16.endIndex)
  expectNil(i16Begin)
  let i16End = u16.index(u16.startIndex, offsetBy: 99, limitedBy: u16.endIndex)
  expectNil(i16End)
}

StringTraps.test("UTF16ViewIndex/offsetCrash")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let s16 = "foo🤦🏻‍♀️"
  let u16 = s16.utf16
  expectCrashLater()
  let i = u16.index(u16.startIndex, offsetBy: 99)
  _ = s16.utf16[i]
}

StringTraps.test("UTF8ViewIndex/offsetLimited")
  .code {
  let sa = "foo"
  let u8a = sa.utf8
  let s8 = sa + "🤦🏻‍♀️"
  let u8 = s8.utf8

  let iaBegin = u8a.index(sa.startIndex, offsetBy: 99, limitedBy: sa.endIndex)
  expectNil(iaBegin)
  let iaEnd = u8a.index(sa.endIndex, offsetBy: 99, limitedBy: sa.endIndex)
  expectNil(iaEnd)
  let i8Begin = u8.index(u8.startIndex, offsetBy: 99, limitedBy: u8.endIndex)
  expectNil(i8Begin)
  let i8End = u8.index(u8.startIndex, offsetBy: 99, limitedBy: u8.endIndex)
  expectNil(i8End)
}

StringTraps.test("UTF8ViewIndex/offsetCrash")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let s8 = "foo🤦🏻‍♀️"
  let u8 = s8.utf8
  expectCrashLater()
  let i = u8.index(u8.startIndex, offsetBy: 99)
  _ = s8.utf8[i]
}

StringTraps.test("UnicodeScalarView index(before:) trap on startIndex")
.skip(
  .custom({ _isFastAssertConfiguration() },
  reason: "trap is not guaranteed to happen in -Ounchecked"))
.code {
  guard #available(SwiftStdlib 5.7, *) else { return }

  let s = "abc"
  var i = s.unicodeScalars.endIndex
  i = s.unicodeScalars.index(before: i)
  i = s.unicodeScalars.index(before: i)
  i = s.unicodeScalars.index(before: i)
  expectCrashLater()
  i = s.unicodeScalars.index(before: i)
}

StringTraps.test("UnicodeScalarView index(before:) trap on startIndex after scalar alignment")
.skip(
  .custom({ _isFastAssertConfiguration() },
  reason: "trap is not guaranteed to happen in -Ounchecked"))
.code {
  guard #available(SwiftStdlib 5.7, *) else { return }

  let s = "🥦 Floret of broccoli"
  var i = s.utf8.index(after: s.utf8.startIndex)
  expectCrashLater()
  // `i` is equivalent to `s.startIndex` as far as `String.UnicodeScalarView` is
  // concerned
  i = s.unicodeScalars.index(before: i)
}

StringTraps.test("UnicodeScalarView index(after:) trap on endIndex")
.skip(
  .custom({ _isFastAssertConfiguration() },
  reason: "trap is not guaranteed to happen in -Ounchecked"))
.code {
  guard #available(SwiftStdlib 5.7, *) else { return }

  let s = "abc"
  var i = s.unicodeScalars.startIndex
  i = s.unicodeScalars.index(after: i)
  i = s.unicodeScalars.index(after: i)
  i = s.unicodeScalars.index(after: i)
  expectCrashLater()
  i = s.unicodeScalars.index(after: i)
}

StringTraps.test("UnicodeScalarView index(after:) trap on i > endIndex")
.skip(
  .custom({ _isFastAssertConfiguration() },
  reason: "trap is not guaranteed to happen in -Ounchecked"))
.code {
  guard #available(SwiftStdlib 5.7, *) else { return }

  let long = "abcd"
  var i = long.unicodeScalars.endIndex

  let s = "abc"
  expectCrashLater()
  i = s.unicodeScalars.index(after: i)
}

StringTraps.test("UnicodeScalarView index(before:) trap on i > endIndex")
.skip(
  .custom({ _isFastAssertConfiguration() },
  reason: "trap is not guaranteed to happen in -Ounchecked"))
.code {
  guard #available(SwiftStdlib 5.7, *) else { return }

  let long = "abcd"
  var i = long.unicodeScalars.endIndex

  let s = "abc"
  expectCrashLater()
  i = s.unicodeScalars.index(before: i)
}

#if _runtime(_ObjC)
StringTraps.test("UTF8View foreign index(after:) trap on i > endIndex")
.skip(
  .custom({ _isFastAssertConfiguration() },
  reason: "trap is not guaranteed to happen in -Ounchecked"))
.code {
  guard #available(SwiftStdlib 5.7, *) else { return }

  let long = "🐘 This is a quite large string, with lots of data"
  let short = ("🐭 I'm much smaller" as NSString) as String

  var i = long.utf8.endIndex
  expectCrashLater()
  // Note: we expect that `short` will be UTF-16 encoded here -- this trap only
  // happens on the foreign path. For native/shared strings, the UTF-8 view's
  // `index(after:)` is essentially doing a simple `i + 1`, like Array does.
  i = short.utf8.index(after: i)
}
#endif

#if _runtime(_ObjC)
StringTraps.test("UTF8View foreign index(before:) trap on i > endIndex")
.skip(
  .custom({ _isFastAssertConfiguration() },
  reason: "trap is not guaranteed to happen in -Ounchecked"))
.code {
  guard #available(SwiftStdlib 5.7, *) else { return }

  let long = "🐘 This is a quite large string, with lots of data"
  let short = ("🐭 I'm much smaller" as NSString) as String

  var i = long.utf8.endIndex
  expectCrashLater()
  // Note: we expect that `short` will be UTF-16 encoded here -- this trap only
  // happens on the foreign path. For native/shared strings, the UTF-8 view's
  // `index(before:)` is essentially doing a simple `i - 1`, like Array does.
  // (Following the unconditional i != startIndex check.)
  i = short.utf8.index(before: i)
}
#endif

#if _runtime(_ObjC)
StringTraps.test("UTF8View foreign index(after:) trap on i == endIndex")
.skip(
  .custom({ _isFastAssertConfiguration() },
  reason: "trap is not guaranteed to happen in -Ounchecked"))
.code {
  guard #available(SwiftStdlib 5.7, *) else { return }

  let s = ("🦧 The Librarian" as NSString) as String

  var i = s.utf8.endIndex
  expectCrashLater()
  // Note: we expect that `short` will be UTF-16 encoded here -- this trap only
  // happens on the foreign path. For native/shared strings, the UTF-8 view's
  // `index(after:)` is essentially doing a simple `i + 1`, like Array does.
  i = s.utf8.index(after: i)
}
#endif

#if _runtime(_ObjC)
StringTraps.test("UTF8View foreign index(before:) trap on i == startIndex")
.skip(
  .custom({ _isFastAssertConfiguration() },
  reason: "trap is not guaranteed to happen in -Ounchecked"))
.code {
  guard #available(SwiftStdlib 5.7, *) else { return }

  let s = ("🦧 The Librarian" as NSString) as String
  var i = s.utf8.startIndex
  expectCrashLater()
  i = s.utf8.index(before: i)
}
#endif
