// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none)
//
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu

import StdlibUnittest
import CxxStdlib

var StdStringOverlayTestSuite = TestSuite("std::string overlay")

StdStringOverlayTestSuite.test("std::string <=> Swift.String") {
  let cxx1 = std.string()
  let swift1 = String(cxxString: cxx1)
  expectEqual(swift1, "")

  let cxx2 = std.string("something123")
  let swift2 = String(cxxString: cxx2)
  expectEqual(swift2, "something123")

  let cxx3: std.string = "literal"
  expectEqual(cxx3.size(), 7)

  // Non-ASCII characters are represented by more than one CChar.
  let cxx4: std.string = "тест"
  expectEqual(cxx4.size(), 8)
  let swift4 = String(cxxString: cxx4)
  expectEqual(swift4, "тест")

  let cxx5: std.string = "emoji_🤖"
  expectEqual(cxx5.size(), 10)
  let swift5 = String(cxxString: cxx5)
  expectEqual(swift5, "emoji_🤖")

  let cxx6 = std.string("xyz\0abc")
  expectEqual(cxx6.size(), 7)
  let swift6 = String(cxxString: cxx6)
  expectEqual(swift6, "xyz\0abc")
}

extension std.string.const_iterator: UnsafeCxxInputIterator {
  // This func should not be required.
  public static func ==(lhs: std.string.const_iterator,
                        rhs: std.string.const_iterator) -> Bool {
#if os(Linux)
    // In libstdc++, `base()` returns UnsafePointer<Optional<UnsafePointer<CChar>>>.
    return lhs.__baseUnsafe().pointee == rhs.__baseUnsafe().pointee
#else
    // In libc++, `base()` returns UnsafePointer<CChar>.
    return lhs.__baseUnsafe() == rhs.__baseUnsafe()
#endif
  }
}

extension std.string: CxxSequence {}

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

runAllTests()
