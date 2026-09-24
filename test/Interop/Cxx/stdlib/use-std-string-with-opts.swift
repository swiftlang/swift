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

runAllTests()
