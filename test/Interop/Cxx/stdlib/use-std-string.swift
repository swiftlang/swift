// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
//
// REQUIRES: executable_test

import StdlibUnittest
import CxxStdlib

var StdStringTestSuite = TestSuite("StdString")

StdStringTestSuite.test("init") {
    let s = std.string()
    expectEqual(s.size(), 0)
    expectTrue(s.empty())
}

StdStringTestSuite.test("push back") {
    var s = std.string()
    s.push_back(42)
    expectEqual(s.size(), 1)
    expectFalse(s.empty())
    expectEqual(s[0], 42)
}

runAllTests()
