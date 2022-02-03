// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test
//
// Enable this everywhere once we have a solution for modularizing libstdc++: rdar://87654514
// REQUIRES: OS=macosx

import StdlibUnittest
import StdString
import std.string

var StdStringTestSuite = TestSuite("StdString")

StdStringTestSuite.test("init") {
    let s = CxxString()
    expectEqual(s.size(), 0)
    expectTrue(s.empty())
}

// LLVM module verification fails for calls to std::string::push_back: rdar://88343327
// StdStringTestSuite.test("push back") {
//     var s = CxxString()
//     s.push_back(42)
//     expectEqual(s.size(), 1)
//     expectFalse(s.empty())
//     expectEqual(s[0], 42)
// }

runAllTests()
