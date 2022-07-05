// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none)
//
// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu

import StdlibUnittest
import std

var StdStringOverlayTestSuite = TestSuite("std::string overlay")

StdStringOverlayTestSuite.test("std::string <=> Swift.String") {
  let cxx1 = std.string()
  let swift1 = String(cxxString: cxx1)
  expectEqual(swift1, "")

  let cxx2 = std.string("something123")
  let swift2 = String(cxxString: cxx2)
  expectEqual(swift2, "something123")
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
