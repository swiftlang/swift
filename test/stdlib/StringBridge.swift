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

runAllTests()
