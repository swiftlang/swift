// RUN: %target-run-simple-swift(-cxx-interoperability-mode=default -Xfrontend -cxx-stdlib-path%S/Inputs/c++ -I %S/Inputs)
//
// REQUIRES: executable_test

import StdlibUnittest
import CxxHeader
import custom_std

var CustomStdTestSuite = TestSuite("CustomStd")

CustomStdTestSuite.test("SimpleStruct") {
    let s = SimpleStruct(x: 0, y: 0)
    expectEqual(s.x, 0)
}

CustomStdTestSuite.test("String") {
    let s = std.string("hello")
    expectEqual(s.size(), 5)
}

runAllTests()
