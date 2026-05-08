// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// rdar://91405760
// UNSUPPORTED: use_os_stdlib, back_deployment_runtime

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

  // https://github.com/apple/swift/issues/52275
  // Corner-case: UBP with null pointer.
  expectEqual(
    "", String(decoding: UnsafeBufferPointer(_empty: ()), as: UTF8.self))
  expectEqual(
    "", String(decoding: UnsafeBufferPointer(_empty: ()), as: UTF16.self))
  expectEqual(
    "", String(decoding: UnsafeBufferPointer(_empty: ()), as: UTF32.self))
}

if #available(SwiftStdlib 5.3, *) {
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
    let longerValidUTF8: [UInt8] = [0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x43, 0x61, 0x66, 0xC3, 0xA9]
    let longerInvalidUTF8: [UInt8] = [0x21, 0x21, 0x43, 0x61, 0x66, 0xC3, 0x43, 0x61, 0x66, 0xC3, 0x43, 0x61, 0x66, 0xC3]
    
    func test(bufferSize: Int, input: [UInt8], expected: String) {
      let strs = (0..<100).map { _ in
        String(unsafeUninitializedCapacity: bufferSize) { buffer in
          if #available(SwiftStdlib 5.10, *) {
            expectEqual(bufferSize, buffer.count)
          }
          _ = buffer.initialize(from: input)
          return input.count
        }
      }
      for str in strs {
        expectEqual(expected, str)
      }
    }
    
    test(bufferSize: validUTF8.count, input: validUTF8, expected: "Café")
    test(bufferSize: invalidUTF8.count, input: invalidUTF8, expected: "Caf�")
    // Force non-smol strings by using a larger capacity
    test(bufferSize: 16, input: validUTF8, expected: "Café")
    test(bufferSize: 16, input: invalidUTF8, expected: "Caf�")
    test(bufferSize: longerValidUTF8.count, input: longerValidUTF8, expected: "!!!!!!!!!!Café")
    test(bufferSize: longerInvalidUTF8.count, input: longerInvalidUTF8, expected: "!!Caf�Caf�Caf�")
    test(bufferSize: 16, input: longerValidUTF8, expected: "!!!!!!!!!!Café")
    test(bufferSize: 16, input: longerInvalidUTF8, expected: "!!Caf�Caf�Caf�")
    let empty = String(unsafeUninitializedCapacity: 16) { _ in
      // Can't initialize the buffer (e.g. the capacity is too small).
      return 0
    }
    expectTrue(empty.isEmpty)
  }
}

if #available(SwiftStdlib 5.3, *) {
  StringCreateTests.test("Small string unsafeUninitializedCapacity") {
    let str1 = "42"
    let str2 = String(42)
    expectEqual(str1, str2)

    let str3 = String(unsafeUninitializedCapacity: 2) {
      $0[0] = UInt8(ascii: "4")
      $0[1] = UInt8(ascii: "2")
      return 2
    }
    expectEqual(str1, str3)

    // Write into uninitialized space.
    let str4 = String(unsafeUninitializedCapacity: 3) {
      $0[0] = UInt8(ascii: "4")
      $0[1] = UInt8(ascii: "2")
      $0[2] = UInt8(ascii: "X")
      // Deinitialize memory used for scratch space before returning.
      ($0.baseAddress! + 2).deinitialize(count: 1)
      return 2
    }
    expectEqual(str1, str4)

    let str5 = String(unsafeUninitializedCapacity: 3) {
      $0[1] = UInt8(ascii: "4")
      $0[2] = UInt8(ascii: "2")
      // Move the initialized UTF-8 code units to the start of the buffer,
      // leaving the non-overlapping source memory uninitialized.
      $0.baseAddress!.moveInitialize(from: $0.baseAddress! + 1, count: 2)
      return 2
    }
    expectEqual(str1, str5)

    // Ensure reasonable behavior despite a deliberate API contract violation.
    let str6 = String(unsafeUninitializedCapacity: 3) {
      $0[0] = UInt8(ascii: "4")
      $0[1] = UInt8(ascii: "2")
      $0[2] = UInt8(ascii: "X")
      return 2
    }
    expectEqual(str1, str6)
  }
}

let s1 = "Long string containing the characters é, ß, 🦆, and 👨‍👧‍👦."
let s2 = "Long ascii string with no accented characters (obviously)."

StringCreateTests.test("Validating.utf8")
.require(.stdlib_6_0)
.code {
  guard #available(SwiftStdlib 6.0, *) else { return }

  let i1 = Array(s1.utf8)
  let i2 = Array(s2.utf8)
  let i3 = {
    var modified = i1
    let index = modified.lastIndex(of: 240)
    expectNotNil(index)
    index.map { modified[$0] = 0 }
    return modified
  }()

  var actual: String?
  for simpleString in SimpleString.allCases {
    let expected = simpleString.rawValue
    actual = String(validating: expected.utf8, as: Unicode.UTF8.self)
    expectEqual(actual, expected)
  }

  expectEqual(String(validating: i1, as: UTF8.self), s1)
  expectEqual(String(validating: i2, as: UTF8.self), s2)
  expectNil(String(validating: i3, as: UTF8.self))

  expectEqual(String(validating: AnyCollection(i1), as: UTF8.self), s1)
  expectEqual(String(validating: AnyCollection(i2), as: UTF8.self), s2)
  expectNil(String(validating: AnyCollection(i3), as: UTF8.self))
}

StringCreateTests.test("Validating.utf8.from.int8")
.require(.stdlib_6_0)
.code {
  guard #available(SwiftStdlib 6.0, *) else { return }

  let i1 = s1.utf8.map(Int8.init(bitPattern:))
  let i2 = s2.utf8.map(Int8.init(bitPattern:))
  let i3 = {
    var modified = i1
    let index = modified.lastIndex(of: Int8(bitPattern: 240))
    expectNotNil(index)
    index.map { modified[$0] = 0 }
    return modified
  }()

  expectEqual(String(validating: i1, as: UTF8.self), s1)
  expectEqual(String(validating: i2, as: UTF8.self), s2)
  expectNil(String(validating: i3, as: UTF8.self))

  expectEqual(String(validating: AnyCollection(i1), as: UTF8.self), s1)
  expectEqual(String(validating: AnyCollection(i2), as: UTF8.self), s2)
  expectNil(String(validating: AnyCollection(i3), as: UTF8.self))
}

StringCreateTests.test("Validating.ascii")
.require(.stdlib_6_0)
.code {
  guard #available(SwiftStdlib 6.0, *) else { return }

  let i1 = Array(s1.utf8)
  let i2 = Array(s2.utf8)

  expectNil(String(validating: i1, as: Unicode.ASCII.self))
  expectEqual(String(validating: i2, as: Unicode.ASCII.self), s2)

  expectNil(String(validating: AnyCollection(i1), as: Unicode.ASCII.self))
  expectEqual(String(validating: AnySequence(i2), as: Unicode.ASCII.self), s2)

  let i3 = i1.map(Int8.init(bitPattern:))
  let i4 = i2.map(Int8.init(bitPattern:))

  expectNil(String(validating: i3, as: Unicode.ASCII.self))
  expectEqual(String(validating: i4, as: Unicode.ASCII.self), s2)

  expectNil(String(validating: AnyCollection(i3), as: Unicode.ASCII.self))
  expectEqual(String(validating: AnySequence(i4), as: Unicode.ASCII.self), s2)
}

StringCreateTests.test("Validating.utf16")
.require(.stdlib_6_0)
.code {
  guard #available(SwiftStdlib 6.0, *) else { return }

  let i1 = Array(s1.utf16)
  let i2 = Array(s2.utf16)
  let i3 = {
    var modified = i1
    let index = modified.lastIndex(of: 32)
    expectNotNil(index)
    index.map { modified[$0] = 0xd801 }
    return modified
  }()

  expectEqual(String(validating: i1, as: UTF16.self), s1)
  expectEqual(String(validating: i2, as: UTF16.self), s2)
  expectNil(String(validating: i3, as: UTF16.self))

  expectEqual(String(validating: AnySequence(i1), as: UTF16.self), s1)
  expectEqual(String(validating: AnySequence(i2), as: UTF16.self), s2)
  expectNil(String(validating: AnyCollection(i3), as: UTF16.self))
}

StringCreateTests.test("UTF16.surrogatePairAtBlockBoundary")
.require(.stdlib_6_0)
.code {
  guard #available(SwiftStdlib 6.0, *) else { return }

  // Place a valid surrogate pair (U+10000) so that the lead sits on, or one
  // before, each of the block-size boundaries the UTF-16 transcoder uses
  // (8 on 32-bit, 16 on 64-bit). Before the relevant fix, a pair straddling the
  // boundary was treated as two unpaired surrogates, which caused
  // String(validating:as:) to return nil for a valid input and
  // String(decoding:as:) to over-allocate (emitting two U+FFFD repairs
  // instead of one U+10000).
  for leadPos in [7, 8, 15, 16, 23, 31] {
    let length = leadPos + 18
    var units: [UInt16] = Array(repeating: 0x0800, count: length)
    units[leadPos] = 0xD800
    units[leadPos + 1] = 0xDC00

    let validated = units.withUnsafeBufferPointer {
      String(validating: $0, as: UTF16.self)
    }
    expectNotNil(validated, "leadPos=\(leadPos)")
    expectEqual(Array(validated?.utf16 ?? "".utf16), units, "leadPos=\(leadPos)")

    let decoded = String(decoding: units, as: UTF16.self)
    expectEqual(Array(decoded.utf16), units, "leadPos=\(leadPos)")
  }
}

StringCreateTests.test("UTF16.surrogatePairFillsBlock")
.require(.stdlib_6_0)
.code {
  guard #available(SwiftStdlib 6.0, *) else { return }

  // Buffer length is exactly the SIMD block size, with one surrogate pair
  // inside and the rest filled with non-ASCII BMP scalars. This verifies that
  // we don't reintroduce a count overflow that was fixed during development of
  // the vectorized path for UTF16 decoding
  for blockSize in [8, 16] {
    for leadPos in 0 ..< (blockSize - 1) {
      var units: [UInt16] = Array(repeating: 0x0800, count: blockSize)
      units[leadPos] = 0xD800
      units[leadPos + 1] = 0xDC00
      let decoded = String(decoding: units, as: UTF16.self)
      expectEqual(
        Array(decoded.utf16), units,
        "blockSize=\(blockSize) leadPos=\(leadPos)")
    }
  }
}

StringCreateTests.test("Validating.utf32")
.require(.stdlib_6_0)
.code {
  guard #available(SwiftStdlib 6.0, *) else { return }

  let i1 = s1.unicodeScalars.map(\.value)
  let i2 = s2.unicodeScalars.map(\.value)
  let i3 = {
    var modified = i1
    let index = modified.lastIndex(of: .init(bitPattern: 32))
    expectNotNil(index)
    index.map { modified[$0] = .max }
    return modified
  }()
  let s4 = SimpleString.emoji.rawValue
  let i4 = s4.unicodeScalars.map(\.value)

  expectEqual(String(validating: i1, as: UTF32.self), s1)
  expectEqual(String(validating: i2, as: UTF32.self), s2)
  expectNil(String(validating: i3, as: UTF32.self))
  expectEqual(String(validating: i4, as: UTF32.self), s4)

  expectEqual(String(validating: AnySequence(i1), as: UTF32.self), s1)
  expectEqual(String(validating: AnySequence(i2), as: UTF32.self), s2)
  expectNil(String(validating: AnyCollection(i3), as: UTF32.self))
  expectEqual(String(validating: AnySequence(i4), as: UTF32.self), s4)
}
