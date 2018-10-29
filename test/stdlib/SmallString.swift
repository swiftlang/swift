// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %s -o %t.out -O
// RUN: %target-run %t.out

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

func verifySmallString(_ small: _SmallString, _ input: String) {
  expectEqual(_SmallString.capacity, small.count + small.unusedCapacity)
  let tiny = Array(input.utf8)
  expectEqual(tiny.count, small.count)
  for (lhs, rhs) in zip(tiny, small) {
    expectEqual(lhs, rhs)
  }

  let smallFromUTF16 = _SmallString(Array(input.utf16))
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

  // Test RAC and Mutable
  var copy = small
  for i in 0..<small.count / 2 {
    let tmp = copy[i]
    copy[i] = copy[copy.count - 1 - i]
    copy[copy.count - 1 - i] = tmp
  }
  expectEqualSequence(small.reversed(), copy)
}

// Testing helper inits
extension _SmallString {
  init?(_ codeUnits: Array<UInt8>) {
    guard let smol = codeUnits.withUnsafeBufferPointer({
      return _SmallString($0)
    }) else {
      return nil
    }
    self = smol
  }
  init?(_ codeUnits: Array<UInt16>) {
    let str = codeUnits.withUnsafeBufferPointer {
      return String._uncheckedFromUTF16($0)
    }
    if !str._guts.isSmall {
      return nil
    }
    self.init(str._guts._object)
  }
  init?(_cocoaString ns: NSString) {
    guard _isObjCTaggedPointer(ns) else { return nil }
    self.init(taggedCocoa: ns)
  }

  func _appending(_ other: _SmallString) -> _SmallString? {
    return _SmallString(self, appending: other)
  }
  func _repeated(_ n: Int) -> _SmallString? {
    var base = self
    let toAppend = self
    for _ in 0..<(n &- 1) {
      guard let s = _SmallString(
        base, appending: toAppend)
      else { return nil }
      base = s
    }
    return base
  }
}

SmallStringTests.test("FitsInSmall") {
  func runTest(_ input: String) throws {
    let tiny = Array(input.utf8)
    // Constructed from UTF-8 code units
    guard let small = _SmallString(tiny) else {
      throw "Didn't fit"
    }
    verifySmallString(small, input)

    // Constructed from UTF-16 code units
    guard let fromUTF16Small = _SmallString(Array(input.utf16)) else {
        throw "Failed from utf-16"
    }
    verifySmallString(fromUTF16Small, input)
  }

  // Pass tests
  //
  expectDoesNotThrow({ try runTest("ab😇c") })
  expectDoesNotThrow({ try runTest("0123456789abcde") })
  expectDoesNotThrow({ try runTest("👨‍👦") })
  expectDoesNotThrow({ try runTest("") })

  // Fail tests
  //
  expectThrows("Didn't fit", { try runTest("0123456789abcdef") })
  expectThrows("Didn't fit", { try runTest("👩‍👦‍👦") })

  for cu in (0 as UInt32)...(0x10FFFF as UInt32) {
    // TODO: Iterate over all scalars when we support UTF-8, and possibly move
    // this to validation suite.
    guard let scalar = Unicode.Scalar(cu) else { continue }
    guard cu <= 0x7F else { break }
    expectDoesNotThrow({ try runTest(String(scalar)) })
  }

}

SmallStringTests.test("Bridging") {
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
  //
  expectDoesNotThrow({ try runTestSmall("abc") })
  expectDoesNotThrow({ try runTestSmall("defghijk") })
  expectDoesNotThrow({ try runTestSmall("aaaaaaaaaaa") })

  // Fail tests
  //
  expectThrows("Didn't fit", { try runTestSmall("\u{0}") })
  expectThrows("Didn't fit", { try runTestSmall("0123456789abcde") })
  expectThrows("Didn't fit", { try runTestSmall("👨‍👦abcd") })
  expectThrows("Didn't fit", { try runTestSmall("👨‍👦") })
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
    _SmallString(Array($0.utf8))
  }
  expectEqual(strings.count, smallstrings.count)
  for (small, str) in zip(smallstrings, strings) {
    verifySmallString(small, str)
  }

  for i in 0..<smallstrings.count {
    for j in i..<smallstrings.count {
      let lhs = smallstrings[i]
      let rhs = smallstrings[j]
      if lhs.count + rhs.count > _SmallString.capacity {
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
