// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

var StaticStringTestSuite = TestSuite("StaticString")

StaticStringTestSuite.test("PointerRepresentation/ASCII/Empty") {
  let str = StaticString()
  expectEqual(0, str.byteSize)
  expectTrue(str.hasPointerRepresentation)
  expectTrue(str.isASCII)
  str.withUTF8Buffer {
    utf8 in
    expectEqual(0, utf8.count)
  }
  expectEqual("", str.stringValue)

  expectPrinted("", str)
  expectDebugPrinted("\"\"", str)
}

StaticStringTestSuite.test("PointerRepresentation/ASCII") {
  let str: StaticString = "abc"
  expectEqual(0x61, str.utf8Start[0])
  expectEqual(0x62, str.utf8Start[1])
  expectEqual(0x63, str.utf8Start[2])
  expectEqual(3, str.byteSize)
  expectTrue(str.hasPointerRepresentation)
  expectTrue(str.isASCII)
  str.withUTF8Buffer {
    (utf8) -> Void in
    expectEqual(3, utf8.count)
    expectEqual(0x61, utf8[0])
    expectEqual(0x62, utf8[1])
    expectEqual(0x63, utf8[2])
  }
  expectEqual("abc", str.stringValue)

  expectPrinted("abc", str)
  expectDebugPrinted("\"abc\"", str)
}

StaticStringTestSuite.test("PointerRepresentation/NonASCII") {
  let str: StaticString = "абв"
  expectEqual(0xd0, str.utf8Start[0])
  expectEqual(0xb0, str.utf8Start[1])
  expectEqual(0xd0, str.utf8Start[2])
  expectEqual(0xb1, str.utf8Start[3])
  expectEqual(0xd0, str.utf8Start[4])
  expectEqual(0xb2, str.utf8Start[5])
  expectEqual(6, str.byteSize)
  expectTrue(str.hasPointerRepresentation)
  expectFalse(str.isASCII)
  str.withUTF8Buffer {
    (utf8) -> Void in
    expectEqual(6, utf8.count)
    expectEqual(0xd0, utf8[0])
    expectEqual(0xb0, utf8[1])
    expectEqual(0xd0, utf8[2])
    expectEqual(0xb1, utf8[3])
    expectEqual(0xd0, utf8[4])
    expectEqual(0xb2, utf8[5])
  }
  expectEqual("абв", str.stringValue)

  expectPrinted("абв", str)
  expectDebugPrinted("\"абв\"", str)
}

StaticStringTestSuite.test("PointerRepresentation/unicodeScalar")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "StaticString should have Unicode scalar representation" : "")
  .code {
  let str: StaticString = "abc"
  let strOpaque = _opaqueIdentity(str)
  expectCrashLater()
  strOpaque.unicodeScalar
}

StaticStringTestSuite.test("UnicodeScalarRepresentation/ASCII") {
  // The type checker does not call the UnicodeScalar initializer even if
  // passed a literal that consists from a single Unicode scalar.

  // U+005A LATIN CAPITAL LETTER Z
  let str = StaticString(_builtinUnicodeScalarLiteral: UInt32(0x5a)._value)
  expectEqual("Z", str.unicodeScalar)
  expectFalse(str.hasPointerRepresentation)
  expectTrue(str.isASCII)
  str.withUTF8Buffer {
    (utf8) -> Void in
    expectEqual(1, utf8.count)
    expectEqual(0x5a, utf8[0])
  }
  expectEqual("Z", str.stringValue)

  expectPrinted("Z", str)
  expectDebugPrinted("\"Z\"", str)
}

StaticStringTestSuite.test("UnicodeScalarRepresentation/NonASCII") {
  // U+042B CYRILLIC CAPITAL LETTER YERU
  let str = StaticString(_builtinUnicodeScalarLiteral: UInt32(0x042b)._value)
  expectEqual("Ы", str.unicodeScalar)
  expectFalse(str.hasPointerRepresentation)
  expectFalse(str.isASCII)
  str.withUTF8Buffer {
    (utf8) -> Void in
    expectEqual(0xd0, utf8[0])
    expectEqual(0xab, utf8[1])
  }
  expectEqual("Ы", str.stringValue)

  expectPrinted("Ы", str)
  expectDebugPrinted("\"Ы\"", str)
}

StaticStringTestSuite.test("UnicodeScalarRepresentation/utf8Start")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "StaticString should have pointer representation" : "")
  .code {
  let str = StaticString(_builtinUnicodeScalarLiteral: UInt32(0x5a)._value)
  let strOpaque = _opaqueIdentity(str)
  expectCrashLater()
  strOpaque.utf8Start
}

StaticStringTestSuite.test("UnicodeScalarRepresentation/byteSize")
  .skip(.Custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches(_isDebugAssertConfiguration() ?
    "StaticString should have pointer representation" : "")
  .code {
  let str = StaticString(_builtinUnicodeScalarLiteral: UInt32(0x5a)._value)
  let strOpaque = _opaqueIdentity(str)
  expectCrashLater()
  strOpaque.byteSize
}

StaticStringTestSuite.test("UnicodeScalarView/round-trip/UTF8") {
  // round-tripping through UnicodeScalarView should return the same value
  let str: StaticString = "абв"
  let str2 = StaticString(str.unicodeScalars)
  expectEqual(str.utf8Start, str2.utf8Start)
  expectEqual(str.byteSize, str2.byteSize)
  expectEqual(str.isASCII, str2.isASCII)
}

StaticStringTestSuite.test("UnicodeScalarView/round-trip/ASCII") {
  // round-tripping through UnicodeScalarView should return the same value
  let str: StaticString = "abc"
  let str2 = StaticString(str.unicodeScalars)
  expectEqual(str.utf8Start, str2.utf8Start)
  expectEqual(str.byteSize, str2.byteSize)
  expectEqual(str.isASCII, str2.isASCII)
}

StaticStringTestSuite.test("UnicodeScalarView/round-trip/Scalar") {
  // round-tripping through UnicodeScalarView should return the same value
  let str: StaticString = StaticString(_builtinUnicodeScalarLiteral: UInt32(0x5a)._value)
  let str2 = StaticString(str.unicodeScalars)
  expectEqual(str.hasPointerRepresentation, str2.hasPointerRepresentation)
  expectEqual(str.unicodeScalar, str2.unicodeScalar)
}

StaticStringTestSuite.test("UnicodeScalarView/generate/NonEmpty") {
  let str: StaticString = "абв"
  expectEqual(["а", "б", "в"], Array(str.unicodeScalars))
}

StaticStringTestSuite.test("UnicodeScalarView/generate/Empty") {
  let str: StaticString = ""
  expectEqual([], Array(str.unicodeScalars))
}

StaticStringTestSuite.test("UnicodeScalarView/generate/Scalar") {
  let str: StaticString = StaticString(_builtinUnicodeScalarLiteral: UInt32(0x5a)._value)
  expectEqual(["Z"], Array(str.unicodeScalars))
}

StaticStringTestSuite.test("UnicodeScalarView/subscript/Pointer") {
  let str: StaticString = "абв"
  let scalars = str.unicodeScalars
  expectEqual("а", scalars[scalars.startIndex])
  expectEqual("б", scalars[scalars.startIndex.advancedBy(1)])
  expectEqual("в", scalars[scalars.startIndex.advancedBy(2)])
  expectEqual(scalars.endIndex, scalars.startIndex.advancedBy(3))
}

StaticStringTestSuite.test("UnicodeScalarView/subscript/Scalar") {
  let str: StaticString = StaticString(_builtinUnicodeScalarLiteral: UInt32(0x5a)._value)
  let scalars = str.unicodeScalars
  expectEqual("Z", scalars[scalars.startIndex])
  expectEqual(scalars.endIndex, scalars.startIndex.advancedBy(1))
}

StaticStringTestSuite.test("UnicodeScalarView/subscript/Empty") {
  let str = StaticString()
  let scalars = str.unicodeScalars
  expectEqual(scalars.startIndex, scalars.endIndex)
}

StaticStringTestSuite.test("UnicodeScalarView/Slice/round-trip/NonEmpty/Pointer") {
  let str: StaticString = "абв"
  let scalars = str.unicodeScalars
  let slice = scalars[scalars.startIndex.successor()...scalars.startIndex.successor()]
  expectEqual("б", StaticString(slice).stringValue)
}

StaticStringTestSuite.test("UnicodeScalarView/Slice/round-trip/NonEmpty/Scalar") {
  let str: StaticString = StaticString(_builtinUnicodeScalarLiteral: UInt32(0x5a)._value)
  let scalars = str.unicodeScalars
  let slice = scalars[scalars.indices]
  expectEqual("Z", StaticString(slice).stringValue)
}

StaticStringTestSuite.test("UnicodeScalarView/Slice/round-trip/Empty/Pointer") {
  let str: StaticString = "abc"
  let scalars = str.unicodeScalars
  let slice = scalars[scalars.startIndex..<scalars.startIndex]
  expectEqual("", StaticString(slice).stringValue)
  expectNotEqual(nil, StaticString(slice).utf8Start)
}

StaticStringTestSuite.test("UnicodeScalarView/Slice/round-trip/Empty/Scalar") {
  let str: StaticString = StaticString(_builtinUnicodeScalarLiteral: UInt32(0x5a)._value)
  let scalars = str.unicodeScalars
  let slice = scalars[scalars.startIndex..<scalars.startIndex]
  expectEqual("", StaticString(slice).stringValue)
  expectNotEqual(nil, StaticString(slice).utf8Start)
}

runAllTests()
