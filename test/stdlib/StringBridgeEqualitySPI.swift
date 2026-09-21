// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %t/main.swift %S/../Inputs/SmallStringTestUtilities.swift -o %t.out -O
// RUN: %target-codesign %t.out
// RUN: %target-run %t.out

// REQUIRES: executable_test

// REQUIRES: objc_interop

// UNSUPPORTED: use_os_stdlib, back_deployment_runtime

// This file has content originally from test/stdlib/StringBridge.swift, but has the above UNSUPPORTED line since the tests in question use symbols not available in some OSs

import Foundation
import StdlibUnittest

var StringBridgeEqualityTests = TestSuite("StringBridgeEqualityTests")

fileprivate extension String {
  func withBytesInEncoding<R>(
    _ encoding: UInt,
    _ work: (UnsafeRawPointer, Int) -> R
  ) -> R {
    let bridged = self as NSString
    return withExtendedLifetime(bridged) {
      let byteCount = bridged.lengthOfBytes(using: encoding)
      let ptr = bridged.cString(using: encoding)
      return work(UnsafeRawPointer(ptr!), byteCount)
    }
  }
}

struct EqualityTestInput: CustomStringConvertible {
  let string: String
  let encoding: UInt
  let bytes: [UInt8]

  var description: String {
    let encodingName = switch encoding {
    case NSASCIIStringEncoding:
      "ASCII"
    case NSUTF8StringEncoding:
      "UTF8"
    case NSUTF16StringEncoding:
      "UTF16"
    default:
      fatalError("Unreachable")
    }
    return "(\(string), scalar count: \(string.unicodeScalars.count), encoding: \(encodingName), bytes: \(bytes))"
  }
}

// We don't run the Foundation tests, so make sure we have coverage for _swift_unicodeBuffersEqual_nonNormalizing ourselves
StringBridgeEqualityTests.test("Foundation Buffer Comparison SPI") {
  let asciiString = "Hello"                           // ASCII (1-byte UTF-8, 2-byte UTF-16)
  let utf8_1byte = "abc"                              // 1-byte UTF-8
  let utf8_2byte = "café"                             // Contains é (2-byte UTF-8)
  let utf8_3byte = "你好"                              // Chinese characters (3-byte UTF-8)
  let utf8_4byte = "𝕳𝖊𝖑𝖑𝖔"                            // Math bold characters (4-byte UTF-8, surrogate pairs in UTF-16)
  let utf16_2byte = "Hello"                           // Basic multilingual plane (2-byte UTF-16)
  let utf16_4byte = "𝕳𝖊𝖑𝖑𝖔"                           // Supplementary plane (4-byte UTF-16 - surrogate pairs)

  let differentString1 = "World"
  let shorterString = "Hi"

  func testEquality(
    _ lhs: String,
    lhsEncoding: UInt,
    _ rhs: String,
    rhsEncoding: UInt,
    expectedEqual: Bool
  ) {
    let (equal, lhsInput, rhsInput) = lhs.withBytesInEncoding(lhsEncoding) { lhsPtr, lhsCount in
      rhs.withBytesInEncoding(rhsEncoding) { rhsPtr, rhsCount in
        let equal = _swift_unicodeBuffersEqual_nonNormalizing(
          bytes: lhsPtr,
          count: lhsCount,
          encoding: lhsEncoding,
          bytes: rhsPtr,
          count: rhsCount,
          encoding: rhsEncoding
        )
        let lhsInput = EqualityTestInput(
          string: lhs,
          encoding: lhsEncoding,
          bytes: Array(UnsafeRawBufferPointer(start: lhsPtr, count: lhsCount)),
        )
        let rhsInput = EqualityTestInput(
          string: rhs,
          encoding: rhsEncoding,
          bytes: Array(UnsafeRawBufferPointer(start: rhsPtr, count: rhsCount)),
        )
        return (equal, lhsInput, rhsInput)
      }
    }
    if expectedEqual {
      expectTrue(equal, "\(rhsInput) and \(lhsInput) should compare equal")
    } else {
      expectFalse(equal, "\(rhsInput) and \(lhsInput) should not compare equal")
    }
  }

  // ASCII vs ASCII - Equal content
  testEquality(asciiString, lhsEncoding: NSASCIIStringEncoding, asciiString, rhsEncoding: NSASCIIStringEncoding, expectedEqual: true)

  // ASCII vs ASCII - Different content, same length
  testEquality(asciiString, lhsEncoding: NSASCIIStringEncoding, differentString1, rhsEncoding: NSASCIIStringEncoding, expectedEqual: false)

  // ASCII vs ASCII - Different content, different length
  testEquality(asciiString, lhsEncoding: NSASCIIStringEncoding, shorterString, rhsEncoding: NSASCIIStringEncoding, expectedEqual: false)

  // ASCII vs UTF-8 - Equal content
  testEquality(asciiString, lhsEncoding: NSASCIIStringEncoding, asciiString, rhsEncoding: NSUTF8StringEncoding, expectedEqual: true)

  // ASCII vs UTF-8 - Different content
  testEquality(asciiString, lhsEncoding: NSASCIIStringEncoding, differentString1, rhsEncoding: NSUTF8StringEncoding, expectedEqual: false)

  // ASCII vs UTF-16 - Equal content
  testEquality(asciiString, lhsEncoding: NSASCIIStringEncoding, asciiString, rhsEncoding: NSUTF16StringEncoding, expectedEqual: true)

  // ASCII vs UTF-16 - Different content
  testEquality(asciiString, lhsEncoding: NSASCIIStringEncoding, differentString1, rhsEncoding: NSUTF16StringEncoding, expectedEqual: false)

  // UTF-8 (1-byte) vs UTF-8 (1-byte) - Equal content
  testEquality(utf8_1byte, lhsEncoding: NSUTF8StringEncoding, utf8_1byte, rhsEncoding: NSUTF8StringEncoding, expectedEqual: true)

  // UTF-8 (1-byte) vs UTF-8 (1-byte) - Different content
  testEquality(utf8_1byte, lhsEncoding: NSUTF8StringEncoding, differentString1, rhsEncoding: NSUTF8StringEncoding, expectedEqual: false)

  // UTF-8 (2-byte) vs UTF-8 (2-byte) - Equal content
  testEquality(utf8_2byte, lhsEncoding: NSUTF8StringEncoding, utf8_2byte, rhsEncoding: NSUTF8StringEncoding, expectedEqual: true)

  // UTF-8 (2-byte) vs UTF-8 (2-byte) - Different content
  testEquality(utf8_2byte, lhsEncoding: NSUTF8StringEncoding, differentString1, rhsEncoding: NSUTF8StringEncoding, expectedEqual: false)

  // UTF-8 (3-byte) vs UTF-8 (3-byte) - Equal content
  testEquality(utf8_3byte, lhsEncoding: NSUTF8StringEncoding, utf8_3byte, rhsEncoding: NSUTF8StringEncoding, expectedEqual: true)

  // UTF-8 (3-byte) vs UTF-8 (3-byte) - Different content
  testEquality(utf8_3byte, lhsEncoding: NSUTF8StringEncoding, utf8_1byte, rhsEncoding: NSUTF8StringEncoding, expectedEqual: false)

  // UTF-8 (4-byte) vs UTF-8 (4-byte) - Equal content
  testEquality(utf8_4byte, lhsEncoding: NSUTF8StringEncoding, utf8_4byte, rhsEncoding: NSUTF8StringEncoding, expectedEqual: true)

  // UTF-8 (4-byte) vs UTF-8 (4-byte) - Different content
  testEquality(utf8_4byte, lhsEncoding: NSUTF8StringEncoding, utf8_3byte, rhsEncoding: NSUTF8StringEncoding, expectedEqual: false)

  // UTF-8 (1-byte) vs UTF-16 (2-byte) - Equal content
  testEquality(utf8_1byte, lhsEncoding: NSUTF8StringEncoding, utf8_1byte, rhsEncoding: NSUTF16StringEncoding, expectedEqual: true)

  // UTF-8 (1-byte) vs UTF-16 (2-byte) - Different content
  testEquality(utf8_1byte, lhsEncoding: NSUTF8StringEncoding, differentString1, rhsEncoding: NSUTF16StringEncoding, expectedEqual: false)

  // UTF-8 (2-byte) vs UTF-16 (2-byte) - Equal content
  testEquality(utf8_2byte, lhsEncoding: NSUTF8StringEncoding, utf8_2byte, rhsEncoding: NSUTF16StringEncoding, expectedEqual: true)

  // UTF-8 (2-byte) vs UTF-16 (2-byte) - Different content
  testEquality(utf8_2byte, lhsEncoding: NSUTF8StringEncoding, utf8_1byte, rhsEncoding: NSUTF16StringEncoding, expectedEqual: false)

  // UTF-8 (3-byte) vs UTF-16 (2-byte) - Equal content
  testEquality(utf8_3byte, lhsEncoding: NSUTF8StringEncoding, utf8_3byte, rhsEncoding: NSUTF16StringEncoding, expectedEqual: true)

  // UTF-8 (3-byte) vs UTF-16 (2-byte) - Different content
  testEquality(utf8_3byte, lhsEncoding: NSUTF8StringEncoding, utf8_2byte, rhsEncoding: NSUTF16StringEncoding, expectedEqual: false)

  // UTF-8 (4-byte) vs UTF-16 (4-byte) - Equal content
  testEquality(utf8_4byte, lhsEncoding: NSUTF8StringEncoding, utf8_4byte, rhsEncoding: NSUTF16StringEncoding, expectedEqual: true)

  // UTF-8 (4-byte) vs UTF-16 (4-byte) - Different content
  testEquality(utf8_4byte, lhsEncoding: NSUTF8StringEncoding, utf8_3byte, rhsEncoding: NSUTF16StringEncoding, expectedEqual: false)

  // UTF-16 (2-byte) vs UTF-16 (2-byte) - Equal content
  testEquality(utf16_2byte, lhsEncoding: NSUTF16StringEncoding, utf16_2byte, rhsEncoding: NSUTF16StringEncoding, expectedEqual: true)

  // UTF-16 (2-byte) vs UTF-16 (2-byte) - Different content
  testEquality(utf16_2byte, lhsEncoding: NSUTF16StringEncoding, differentString1, rhsEncoding: NSUTF16StringEncoding, expectedEqual: false)

  // UTF-16 (4-byte) vs UTF-16 (4-byte) - Equal content
  testEquality(utf16_4byte, lhsEncoding: NSUTF16StringEncoding, utf16_4byte, rhsEncoding: NSUTF16StringEncoding, expectedEqual: true)

  // UTF-16 (4-byte) vs UTF-16 (4-byte) - Different content
  testEquality(utf16_4byte, lhsEncoding: NSUTF16StringEncoding, utf16_2byte, rhsEncoding: NSUTF16StringEncoding, expectedEqual: false)

  // UTF-16 (2-byte) vs UTF-8 (1-byte) - Equal content (reverse of test 16)
  testEquality(utf8_1byte, lhsEncoding: NSUTF16StringEncoding, utf8_1byte, rhsEncoding: NSUTF8StringEncoding, expectedEqual: true)

  // UTF-16 (2-byte) vs UTF-8 (2-byte) - Equal content (reverse of test 18)
  testEquality(utf8_2byte, lhsEncoding: NSUTF16StringEncoding, utf8_2byte, rhsEncoding: NSUTF8StringEncoding, expectedEqual: true)

  // UTF-16 (2-byte) vs UTF-8 (3-byte) - Equal content (reverse of test 20)
  testEquality(utf8_3byte, lhsEncoding: NSUTF16StringEncoding, utf8_3byte, rhsEncoding: NSUTF8StringEncoding, expectedEqual: true)

  // UTF-16 (4-byte) vs UTF-8 (4-byte) - Equal content (reverse of test 22)
  testEquality(utf8_4byte, lhsEncoding: NSUTF16StringEncoding, utf8_4byte, rhsEncoding: NSUTF8StringEncoding, expectedEqual: true)

  // UTF-16 vs ASCII - Equal content (reverse of test 6)
  testEquality(asciiString, lhsEncoding: NSUTF16StringEncoding, asciiString, rhsEncoding: NSASCIIStringEncoding, expectedEqual: true)

  // Empty strings - ASCII vs ASCII
  testEquality("", lhsEncoding: NSASCIIStringEncoding, "", rhsEncoding: NSASCIIStringEncoding, expectedEqual: true)

  // Empty strings - UTF-8 vs UTF-16
  testEquality("", lhsEncoding: NSUTF8StringEncoding, "", rhsEncoding: NSUTF16StringEncoding, expectedEqual: true)

  // Empty vs non-empty
  testEquality("", lhsEncoding: NSUTF8StringEncoding, asciiString, rhsEncoding: NSUTF8StringEncoding, expectedEqual: false)

  // Different lengths - UTF-8 vs UTF-16
  testEquality(asciiString, lhsEncoding: NSUTF8StringEncoding, shorterString, rhsEncoding: NSUTF16StringEncoding, expectedEqual: false)
}

runAllTests()
