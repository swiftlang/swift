// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %t/main.swift %S/../Inputs/SmallStringTestUtilities.swift -o %t.out -O
// RUN: %target-codesign %t.out
// RUN: %target-run %t.out

// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

var StringBridgeTests = TestSuite("StringBridgeTests")

extension String {
  init(fromCocoa s: String) {
    self = (s as NSString) as String
  }


}

StringBridgeTests.test("Tagged NSString") {
  guard #available(macOS 10.13, iOS 11.0, tvOS 11.0, *) else { return }
#if _pointerBitWidth(_64)
  // Bridge tagged strings as small
  expectSmall((("0123456" as NSString) as String))
  expectSmall((("012345678" as NSString) as String))
  expectSmall((("aaaaaaaaaaa" as NSString) as String))
  expectSmall((("bbbbbbbbb" as NSString) as String))

  // Bridge non-tagged as non-small even if they fit, for fear of losing
  // associated information
  let bigAs = ("aaaaaaaaaaaa" as NSString) as String
  let bigBs = ("bbbbbbbbbb" as NSString) as String
  let bigQs = ("????????" as NSString) as String
  expectCocoa(bigAs)
  expectCocoa(bigBs)
  expectCocoa(bigQs)

#if false // FIXME: Re-enable (https://github.com/apple/swift/issues/50136)
  let littleAsNSString = ("aa" as NSString)
  var littleAs = littleAsNSString as String

  // But become small when appended to
  expectSmall(bigAs + "c")
  expectSmall(bigBs + "c")
  expectSmall("a\(bigAs)")
  expectSmall("a\(bigBs)")
  expectSmall(littleAs + bigQs)
  expectSmall(bigQs + littleAs)
  expectSmall("\(littleAs)bigQs\(littleAs)")
#endif // false

#endif // not 32bit
}

StringBridgeTests.test("Constant NSString New SPI") {
  if #available(SwiftStdlib 6.1, *) {
    //21 characters long so avoids _SmallString
    let constantString:NSString = CFRunLoopMode.commonModes.rawValue as NSString
    let regularBridged = constantString as String
    let count = regularBridged.count
    let bridged = String(
      _immortalCocoaString: constantString,
      count: count,
      encoding: Unicode.ASCII.self
    )
    let reverseBridged = bridged as NSString
    expectEqual(constantString, reverseBridged)
    expectEqual(
      ObjectIdentifier(constantString),
      ObjectIdentifier(reverseBridged)
    )
    expectEqual(bridged, regularBridged)
  }
}

StringBridgeTests.test("Shared String SPI")
  .require(.stdlib_6_2)
  .code {
    guard #available(SwiftStdlib 6.2, *) else { return }
    func test(literal: String, isASCII: Bool) {
      let baseCount = literal.utf8.count
      literal.withCString { intptr in
        intptr.withMemoryRebound(to: UInt8.self, capacity: baseCount) { ptr in
          let fullBuffer = UnsafeBufferPointer(start: ptr, count: baseCount)
          let fullString = _SwiftCreateImmortalString_ForFoundation(
            buffer: fullBuffer,
            isASCII: isASCII
          )
          expectNotNil(fullString)
          let bridgedFullString = (fullString! as NSString)
          let fullCString = bridgedFullString.utf8String!
          expectEqual(baseCount, strlen(fullCString))
          expectEqual(strcmp(ptr, fullCString), 0)
          let fullCString2 = bridgedFullString.utf8String!
          expectEqual(fullCString, fullCString2) //if we're already terminated, we can return the contents pointer as-is
          withExtendedLifetime(fullString) {}
        }
      }
    }
    test(literal: "abcdefghijklmnopqrstuvwxyz", isASCII: true)
    test(literal: "abcdëfghijklmnopqrstuvwxyz", isASCII: false)
}

StringBridgeTests.test("Bridging") {
  // Test bridging retains small string form
  func bridge(_ small: _SmallString) -> String {
    return String(_StringGuts(small))._bridgeToObjectiveCImpl() as! String
  }
  func runTestSmall(_ input: String) throws {
    // Constructed through CF
    guard let fromCocoaSmall = _SmallString(
      _cocoaString: input as NSString
    ) else {
        throw "Didn't fit"
    }
    verifySmallString(fromCocoaSmall, input)
    verifySmallString(fromCocoaSmall, bridge(fromCocoaSmall))
  }

  // Pass tests

  #if _pointerBitWidth(_64)
  if #available(macOS 10.15, iOS 13, *) {
    expectDoesNotThrow({ try runTestSmall("abc") })
    expectDoesNotThrow({ try runTestSmall("defghijk") })
    expectDoesNotThrow({ try runTestSmall("aaaaaaaaaaa") })
  } else if #available(macOS 10.10, iOS 9, *) {
    // FIXME: tests temporarily disabled on these OS versions
    // due to failing in CI. rdar://problem/54875979
  } else {
    // OS X 10.9, iOS 7/8 did not have tagged strings
    expectThrows("Didn't fit", { try runTestSmall("abc") })
    expectThrows("Didn't fit", { try runTestSmall("defghijk") })
    expectThrows("Didn't fit", { try runTestSmall("aaaaaaaaaaa") })
  }
  #endif

  // Fail tests
  //
  expectThrows("Didn't fit", { try runTestSmall("\u{0}") })
  expectThrows("Didn't fit", { try runTestSmall("0123456789abcde") })
  expectThrows("Didn't fit", { try runTestSmall("👨‍👦abcd") })
  expectThrows("Didn't fit", { try runTestSmall("👨‍👦") })
  expectThrows("Didn't fit", { try runTestSmall("👨‍👩‍👦") })
  expectThrows("Didn't fit", { try runTestSmall("👨‍👦abcde") })
}

func returnOne<T>(_ t: T) -> Int { return 1 }
StringBridgeTests.test("Character from NSString") {
  guard #available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *) else { return }

  // NOTE: Using hard-coded literals to directly construct NSStrings
  let ns1 = "A" as NSString
  let ns2 = "A\u{301}" as NSString
  let ns3 = "𓁹͇͈͉͍͎͊͋͌ͧͨͩͪͫͬͭͮ͏̛͓͔͕͖͙͚̗̘̙̜̹̺̻̼͐͑͒͗͛ͣͤͥͦ̽̾̿̀́͂̓̈́͆ͧͨͩͪͫͬͭͮ͘̚͜͟͢͝͞͠͡ͅ" as NSString

  let c1 = Character(ns1 as String)
  let c2 = Character(ns2 as String)
  let c3 = Character(ns3 as String)

  expectEqual("A", String(c1))
  expectNotNil(String(c1).utf8.withContiguousStorageIfAvailable(returnOne))

  expectEqual("A\u{301}", String(c2))
  expectNotNil(String(c2).utf8.withContiguousStorageIfAvailable(returnOne))
  expectNil((ns2 as String).utf8.withContiguousStorageIfAvailable(returnOne))

  expectEqual("𓁹͇͈͉͍͎͊͋͌ͧͨͩͪͫͬͭͮ͏̛͓͔͕͖͙͚̗̘̙̜̹̺̻̼͐͑͒͗͛ͣͤͥͦ̽̾̿̀́͂̓̈́͆ͧͨͩͪͫͬͭͮ͘̚͜͟͢͝͞͠͡ͅ", String(c3))
  expectNotNil(String(c3).utf8.withContiguousStorageIfAvailable(returnOne))
  expectNil((ns3 as String).utf8.withContiguousStorageIfAvailable(returnOne))
}

StringBridgeTests.test("lengthOfBytes(using:)") {
  let ascii = "The quick brown fox jumps over the lazy dog"
  let utf8 = "The quick brown fox jümps over the lazy dog"
  let asciiAsASCIILen = ascii.lengthOfBytes(using: .ascii)
  let asciiAsUTF8Len = ascii.lengthOfBytes(using: .utf8)
  let asciiAsUTF16Len = ascii.lengthOfBytes(using: .utf16)
  let asciiAsMacRomanLen = ascii.lengthOfBytes(using: .macOSRoman)
  let utf8AsASCIILen = utf8.lengthOfBytes(using: .ascii)
  let utf8AsUTF8Len = utf8.lengthOfBytes(using: .utf8)
  let utf8AsUTF16Len = utf8.lengthOfBytes(using: .utf16)
  let utf8AsMacRomanLen = utf8.lengthOfBytes(using: .macOSRoman)
  expectEqual(asciiAsASCIILen, 43)
  expectEqual(asciiAsUTF8Len, 43)
  expectEqual(asciiAsUTF16Len, 86)
  expectEqual(asciiAsMacRomanLen, 43)
  expectEqual(utf8AsASCIILen, 0)
  expectEqual(utf8AsUTF8Len, 44)
  expectEqual(utf8AsUTF16Len, 86)
  expectEqual(utf8AsMacRomanLen, 43)
}

StringBridgeTests.test("Equal UTF16 lengths but unequal UTF8") {
  let utf8 = "чебурашка@ящик-с-апельсинами.рф" //Native, UTF16 count: 31, UTF8 count: 58
  let nsascii = "2166002315@874404110.1042078977" as NSString //Non-native, UTF16 count: 31, UTF8 count: 31
  let nsutf8 = String(decoding: utf8.utf8, as: UTF8.self) as NSString //bridged native
  expectFalse(nsutf8.isEqual(nsascii))
}

fileprivate extension String {
  func withBytesInEncoding<R>(
    _ encoding: UInt,
    _ work: (UnsafeRawPointer, Int) -> R
  ) -> R {
    if encoding == NSASCIIStringEncoding || encoding == NSUTF8StringEncoding {
      return Array(self.utf8).withUnsafeBufferPointer { buffer in
        return work(buffer.baseAddress!, count)
      }
    }
    if encoding == NSUTF16StringEncoding {
      return Array(self.utf16).withUnsafeBufferPointer { utf16Buffer in
        utf16Buffer.withMemoryRebound(to: UInt8.self) { buffer in
          work(buffer.baseAddress!, count)
        }
      }
    }
    fatalError("Unsupported encoding")
  }
}

// We don't run the Foundation tests, so make sure we have coverage for _unicodeBuffersEqual ourselves
StringBridgeTests.test("Foundation Buffer Comparison SPI") {
  let asciiString = "Hello"                           // ASCII (1-byte UTF-8, 2-byte UTF-16)
  let utf8_1byte = "abc"                              // 1-byte UTF-8
  let utf8_2byte = "café"                             // Contains é (2-byte UTF-8)
  let utf8_3byte = "你好"                              // Chinese characters (3-byte UTF-8)
  let utf8_4byte = "𝕳𝖊𝖑𝖑𝖔"                            // Math bold characters (4-byte UTF-8, surrogate pairs in UTF-16)
  let utf16_2byte = "Hello"                           // Basic multilingual plane (2-byte UTF-16)
  let utf16_4byte = "𝕳𝖊𝖑𝖑𝖔"                           // Supplementary plane (4-byte UTF-16 - surrogate pairs)
  
  let differentString1 = "World"
  let differentString2 = "Different"
  let shorterString = "Hi"
    
  func testEquality(
    _ lhs: String,
    lhsEncoding: UInt,
    _ rhs: String,
    rhsEncoding: UInt,
    expectedEqual: Bool
  ) {
    let result = lhs.withBytesInEncoding(lhsEncoding) { lhsPtr, lhsCount in
      rhs.withBytesInEncoding(rhsEncoding) { rhsPtr, rhsCount in
        _unicodeBuffersEqual(
          bytes: lhsPtr,
          count: lhsCount,
          encoding: lhsEncoding,
          bytes: rhsPtr,
          count: rhsCount,
          encoding: rhsEncoding
        )
      }
    }
    
    expectEqual(result, expectedEqual)
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
