// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
defer { runAllTests() }

var StringCreateTests = TestSuite("StringCreateTests")

enum SimpleString: String {
  case smallASCII = "abcdefg"
  case smallUnicode = "abéÏ𓀀"
  case largeASCII = "012345678901234567890"
  case largeUnicode = "abéÏ012345678901234567890𓀀"
  case emoji = "😀😃🤢🤮👩🏿‍🎤🧛🏻‍♂️🧛🏻‍♂️👩‍👩‍👦‍👦"
}

let simpleStrings: [String] = [
    SimpleString.smallASCII.rawValue,
    SimpleString.smallUnicode.rawValue,
    SimpleString.largeASCII.rawValue,
    SimpleString.largeUnicode.rawValue,
    SimpleString.emoji.rawValue,
    "",
]

extension String {
  var utf32: [UInt32] { return unicodeScalars.map { $0.value } }
}

StringCreateTests.test("String(decoding:as)") {
  func validateDecodingAs(_ str: String) {
    // Non-contiguous (maybe) storage
    expectEqual(str, String(decoding: str.utf8, as: UTF8.self))
    expectEqual(str, String(decoding: str.utf16, as: UTF16.self))
    expectEqual(str, String(decoding: str.utf32, as: UTF32.self))

    // Contiguous storage
    expectEqual(str, String(decoding: Array(str.utf8), as: UTF8.self))
    expectEqual(str, String(decoding: Array(str.utf16), as: UTF16.self))
    expectEqual(str, String(decoding: Array(str.utf32), as: UTF32.self))

  }

  for str in simpleStrings {
    validateDecodingAs(str)
  }

  // Corner-case: UBP with null pointer (https://bugs.swift.org/browse/SR-9869)
  expectEqual(
    "", String(decoding: UnsafeBufferPointer(_empty: ()), as: UTF8.self))
  expectEqual(
    "", String(decoding: UnsafeBufferPointer(_empty: ()), as: UTF16.self))
  expectEqual(
    "", String(decoding: UnsafeBufferPointer(_empty: ()), as: UTF32.self))
}

