// RUN: %target-run-simple-swift(-I %S/Inputs -O -enable-experimental-cxx-interop)
// REQUIRES: executable_test

import CxxStdlib
import StdlibUnittest

var CxxDictionaryTestSuite = TestSuite("CxxDictionaryOptimized")

CxxDictionaryTestSuite.test("Dictionary with std::string Key") {
  var r = [std.string: Int]()
  r[std.string("x")] = 1
  expectEqual(r[std.string("x")], 1)
}

CxxDictionaryTestSuite.test("Set with std::string Element") {
  var s = Set<std.string>()
  s.insert(std.string("x"))
  expectTrue(s.contains(std.string("x")))
}

runAllTests()
