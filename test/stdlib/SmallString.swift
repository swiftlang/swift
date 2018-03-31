// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: CPU=arm64 || CPU=x86_64

//
// Tests for small strings
//

import StdlibUnittest
import Foundation
var SmallStringTests = TestSuite("SmallStringTests")

extension String: Error {}

func verifySmallString(_ small: _SmallUTF8String, _ input: String) {
  expectEqual(_SmallUTF8String.capacity, small.count + small.unusedCapacity)
  let tiny = Array(input.utf8)
  expectEqual(tiny.count, small.count)
  for (lhs, rhs) in zip(tiny, small) {
    expectEqual(lhs, rhs)
  }
  small.withTranscodedUTF16CodeUnits {
    let codeUnits = Array(input.utf16)
    expectEqualSequence(codeUnits, $0)
  }

  let smallFromUTF16 = _SmallUTF8String(Array(input.utf16))
  expectNotNil(smallFromUTF16)
  expectEqualSequence(small, smallFromUTF16!)

  // Test slicing
  //
  for i in 0..<small.count {
    for j in i...small.count {
      expectEqualSequence(tiny[i..<j], small[i..<j])
      if j < small.count {
        expectEqualSequence(tiny[i...j], small[i...j])
      }
    }
  }
}

SmallStringTests.test("FitsInSmall") {
  func runTest(_ input: String) throws {
    let tiny = Array(input.utf8)
    // Constructed from UTF-8 code units
    guard let small = _SmallUTF8String(tiny) else {
      throw "Didn't fit"
    }
    verifySmallString(small, input)

    // Constructed from UTF-16 code units
    guard let fromUTF16Small = _SmallUTF8String(Array(input.utf16)) else {
        throw "Failed from utf-16"
    }
    verifySmallString(fromUTF16Small, input)
  }

  // Pass tests
  //
  // TODO(UTF-8 SSO): expectDoesNotThrow({ try runTest("ab😇c") })
  expectDoesNotThrow({ try runTest("0123456789abcde") })
  // TODO(UTF-8 SSO): expectDoesNotThrow({ try runTest("👨‍👦") })
  expectDoesNotThrow({ try runTest("") })

  // Fail tests
  //
  expectThrows("Didn't fit", { try runTest("0123456789abcdef") })
  expectThrows("Didn't fit", { try runTest("👩‍👦‍👦") })
}

SmallStringTests.test("Bridging") {
  // Test bridging retains small string form
  func bridge(_ small: _SmallUTF8String) -> String {
    return _bridgeToCocoa(small) as! String
  }
  func runTestSmall(_ input: String) throws {
    // Constructed through CF
    guard let fromCocoaSmall = _SmallUTF8String(
      _cocoaString: input as NSString
    ) else {
        throw "Didn't fit"
    }
    verifySmallString(fromCocoaSmall, input)
    verifySmallString(fromCocoaSmall, bridge(fromCocoaSmall))
  }

  // Pass tests
  //
  expectDoesNotThrow({ try runTestSmall("abc") })
  expectDoesNotThrow({ try runTestSmall("0123456789abcde") })
  expectDoesNotThrow({ try runTestSmall("\u{0}") })
  // TODO(UTF-8 SSO): expectDoesNotThrow({ try runTestSmall("👨‍👦") })
  expectDoesNotThrow({ try runTestSmall("") })
  // TODO(UTF-8 SSO): expectDoesNotThrow({ try runTestSmall("👨‍👦abcd") })

  // Fail tests
  //
  expectThrows("Didn't fit", { try runTestSmall("👨‍👩‍👦") })
  expectThrows("Didn't fit", { try runTestSmall("👨‍👦abcde") })
}

SmallStringTests.test("Append, repeating") {
  let strings = [
    "",
    "a",
    "bc",
    "def",
    "hijk",
    "lmnop",
    "qrstuv",
    "xyzzzzz",
    "01234567",
    "890123456",
    "7890123456",
    "78901234567",
    "890123456789",
    "0123456789012",
    "34567890123456",
    "789012345678901",
    ]
  let smallstrings = strings.compactMap {
    _SmallUTF8String(Array($0.utf8))
  }
  expectEqual(strings.count, smallstrings.count)
  for (small, str) in zip(smallstrings, strings) {
    verifySmallString(small, str)
  }

  for i in 0..<smallstrings.count {
    for j in i..<smallstrings.count {
      let lhs = smallstrings[i]
      let rhs = smallstrings[j]
      if lhs.count + rhs.count > _SmallUTF8String.capacity {
        continue
      }
      verifySmallString(lhs._appending(rhs)!, (strings[i] + strings[j]))
      verifySmallString(rhs._appending(lhs)!, (strings[j] + strings[i]))
    }
  }

  for i in 0..<smallstrings.count {
    for c in 2...15 {
      let str = String(repeating: strings[i], count: c)
      if let small = smallstrings[i]._repeated(c) {
        verifySmallString(small, str)
      } else {
        expectTrue(str.utf8.count > 15)
      }
    }
  }
}

runAllTests()
