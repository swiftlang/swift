// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -disable-access-control -module-name a %t/main.swift %S/../Inputs/SmallStringTestUtilities.swift -o %t.out -O
// RUN: %target-run %t.out

// REQUIRES: executable_test
// REQUIRES: CPU=arm64 || CPU=x86_64

//
// Tests for small strings
//

import StdlibUnittest
#if _runtime(_ObjC)
import Foundation
#endif
var SmallStringTests = TestSuite("SmallStringTests")
  
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
