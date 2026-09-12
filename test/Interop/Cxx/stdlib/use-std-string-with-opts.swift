// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xcc -std=c++20 -O)
//
// REQUIRES: executable_test
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

// Tests optimizations related to CxxStdlib.

import StdlibUnittest
import CxxStdlib
import StdStringAndVector

var StdStringOptTestSuite = TestSuite("StdStringWithOpts")

StdStringOptTestSuite.test("std::string with Hashable conformance optimized") {
    let item = get_item()
    let dict = Dictionary(uniqueKeysWithValues: zip(item.keys, item.values).lazy)

    expectEqual(dict.count, 0)
}

StdStringOptTestSuite.test("Dictionary with std::string Key") {
  var r = [std.string: Int]()
  r[std.string("x")] = 1
  expectEqual(r[std.string("x")], 1)
}

StdStringOptTestSuite.test("Set with std::string Element") {
  var s = Set<std.string>()
  s.insert(std.string("x"))
  expectTrue(s.contains(std.string("x")))
}

runAllTests()
