// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none)
//
// REQUIRES: executable_test

import StdlibUnittest
import StdString
#if os(Linux)
import CxxStdlib
// FIXME: import CxxStdlib.string once libstdc++ is split into submodules.
#else
import CxxStdlib.string
#endif

var StdStringTestSuite = TestSuite("StdString")

StdStringTestSuite.test("init") {
    let s = CxxString()
    expectEqual(s.size(), 0)
    expectTrue(s.empty())
}

StdStringTestSuite.test("push back") {
    var s = CxxString()
    s.push_back(42)
    expectEqual(s.size(), 1)
    expectFalse(s.empty())
    expectEqual(s[0], 42)
}

runAllTests()
