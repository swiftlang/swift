// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
defer { runAllTests() }

var StringCreateTests = TestSuite("StringCreateTests")

enum SimpleString: String, CaseIterable {
  case smallASCII = "abcdefg"
  case smallUnicode = "abéÏ𓀀"
  case largeASCII = "012345678901234567890"
  case largeUnicode = "abéÏ012345678901234567890𓀀"
  case emoji = "😀😃🤢🤮👩🏿‍🎤🧛🏻‍♂️🧛🏻‍♂️👩‍👩‍👦‍👦"
  case empty = ""
}

extension String {
  var utf32: [UInt32] { return unicodeScalars.map { $0.value } }
}

StringCreateTests.test("String(decoding:as:)") {
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

  for simpleString in SimpleString.allCases {
    validateDecodingAs(simpleString.rawValue)
  }

  // Corner-case: UBP with null pointer (https://bugs.swift.org/browse/SR-9869)
  expectEqual(
    "", String(decoding: UnsafeBufferPointer(_empty: ()), as: UTF8.self))
  expectEqual(
    "", String(decoding: UnsafeBufferPointer(_empty: ()), as: UTF16.self))
  expectEqual(
    "", String(decoding: UnsafeBufferPointer(_empty: ()), as: UTF32.self))
}

if #available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *) {
  StringCreateTests.test("String(unsafeUninitializedCapacity:initializingUTF8With:)") {
    for simpleString in SimpleString.allCases {
      let expected = simpleString.rawValue
      let expectedUTF8 = expected.utf8
      let actual = String(unsafeUninitializedCapacity: expectedUTF8.count) {
        _ = $0.initialize(from: expectedUTF8)
        return expectedUTF8.count
      }
      expectEqual(expected, actual)
    }

    let validUTF8: [UInt8] = [0x43, 0x61, 0x66, 0xC3, 0xA9]
    let invalidUTF8: [UInt8] = [0x43, 0x61, 0x66, 0xC3]

    let cafe1 = String(unsafeUninitializedCapacity: validUTF8.count) {
      _ = $0.initialize(from: validUTF8)
      return validUTF8.count
    }
    expectEqual("Café", cafe1)

    let cafe2 = String(unsafeUninitializedCapacity: invalidUTF8.count) {
      _ = $0.initialize(from: invalidUTF8)
      return invalidUTF8.count
    }
    expectEqual("Caf�", cafe2)

    let empty = String(unsafeUninitializedCapacity: 16) { _ in
      // Can't initialize the buffer (e.g. the capacity is too small).
      return 0
    }
    expectTrue(empty.isEmpty)
  }
}
