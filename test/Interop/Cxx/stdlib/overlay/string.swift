// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-cxx-interop)
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

runAllTests()
