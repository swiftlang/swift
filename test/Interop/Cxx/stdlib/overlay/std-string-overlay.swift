// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none)
//
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu

import StdlibUnittest
import CxxStdlib

var StdStringOverlayTestSuite = TestSuite("std::string overlay")

StdStringOverlayTestSuite.test("std::string <=> Swift.String") {
  let cxx1 = std.string()
  let swift1 = String(cxx1)
  expectEqual(swift1, "")

  let cxx2 = std.string("something123")
  let swift2 = String(cxx2)
  expectEqual(swift2, "something123")

  let cxx3: std.string = "literal"
  expectEqual(cxx3.size(), 7)

  // Non-ASCII characters are represented by more than one CChar.
  let cxx4: std.string = "тест"
  expectEqual(cxx4.size(), 8)
  let swift4 = String(cxx4)
  expectEqual(swift4, "тест")

  let cxx5: std.string = "emoji_🤖"
  expectEqual(cxx5.size(), 10)
  let swift5 = String(cxx5)
  expectEqual(swift5, "emoji_🤖")

  let cxx6 = std.string("xyz\0abc")
  expectEqual(cxx6.size(), 7)
  let swift6 = String(cxx6)
  expectEqual(swift6, "xyz\0abc")

  var cxx7 = std.string()
  let bytes: [UInt8] = [0xE1, 0xC1, 0xAC]
  for byte in bytes {
    cxx7.push_back(CChar(bitPattern: byte))
  }
  let swift7 = String(cxx7)
  expectEqual(swift7, "���")
}

StdStringOverlayTestSuite.test("std::u16string <=> Swift.String") {
  let cxx1 = std.u16string()
  let swift1 = String(cxx1)
  expectEqual(swift1, "")

  let cxx2 = std.u16string("something123")
  expectEqual(cxx2.size(), 12)
  let swift2 = String(cxx2)
  expectEqual(swift2, "something123")

  let cxx3: std.u16string = "literal"
  expectEqual(cxx3.size(), 7)

  let cxx4: std.u16string = "тест"
  expectEqual(cxx4.size(), 4)
  let swift4 = String(cxx4)
  expectEqual(swift4, "тест")

  // Emojis are represented by more than one CWideChar.
  let cxx5: std.u16string = "emoji_🤖"
  expectEqual(cxx5.size(), 8)
  let swift5 = String(cxx5)
  expectEqual(swift5, "emoji_🤖")

  let cxx6 = std.u16string("xyz\0abc")
  expectEqual(cxx6.size(), 7)
  let swift6 = String(cxx6)
  expectEqual(swift6, "xyz\0abc")
}

StdStringOverlayTestSuite.test("std::string as Swift.CustomDebugStringConvertible") {
  let cxx1 = std.string()
  expectEqual(cxx1.debugDescription, "std.string()")

  let cxx2 = std.string("something123")
  expectEqual(cxx2.debugDescription, "std.string(something123)")

  let bytes: [UInt8] = [0xE1, 0xC1, 0xAC]
  var cxx3 = std.string()
  for byte in bytes {
    cxx3.push_back(CChar(bitPattern: byte))
  }
  expectEqual(cxx3.debugDescription, "std.string(���)")
}

StdStringOverlayTestSuite.test("std::u16string as Swift.CustomDebugStringConvertible") {
  let cxx1 = std.u16string()
  expectEqual(cxx1.debugDescription, "std.u16string()")

  let cxx2 = std.u16string("something123")
  expectEqual(cxx2.debugDescription, "std.u16string(something123)")

  let scalars: [UInt16] = [97, 55296, 99]
  var cxx3 = std.u16string()
  for scalar in scalars {
    cxx3.push_back(scalar)
  }
  expectEqual(cxx3.debugDescription, "std.u16string(a�c)")
}

StdStringOverlayTestSuite.test("std::string as Swift.Sequence") {
  let cxx1 = std.string()
  var iterated = false
  for _ in cxx1 {
    iterated = true
  }
  expectFalse(iterated)

  let cxx2 = std.string("abc123")
  var chars = 0
  var sum = 0
  for it in cxx2 {
    chars += 1
    sum += Int(it)
  }
  expectEqual(6, chars)
  expectEqual(97 + 98 + 99 + 49 + 50 + 51, sum)
}

StdStringOverlayTestSuite.test("std::string as CustomStringConvertible") {
  let cxx1 = std.string()
  expectEqual(cxx1.description, "")

  let cxx2 = std.string("something123")
  expectEqual(cxx2.description, "something123")

  let bytes: [UInt8] = [0xE1, 0xC1, 0xAC]
  var cxx3 = std.string()
  for byte in bytes {
    cxx3.push_back(CChar(bitPattern: byte))
  }
  expectEqual(cxx3.description, "���")
}

StdStringOverlayTestSuite.test("std::u16string as Swift.CustomStringConvertible") {
  let cxx1 = std.u16string()
  expectEqual(cxx1.description, "")

  let cxx2 = std.u16string("something123")
  expectEqual(cxx2.description, "something123")

  let scalars: [UInt16] = [97, 55296, 99]
  var cxx3 = std.u16string()
  for scalar in scalars {
    cxx3.push_back(scalar)
  }
  expectEqual(cxx3.description, "a�c")
}

runAllTests()
