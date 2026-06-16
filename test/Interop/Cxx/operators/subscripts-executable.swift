// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import Subscripts

var FieldsTestSuite = TestSuite("Getting and setting subscripts in base classes")

FieldsTestSuite.test("Subscript from derived returning ref") {
    var derived = DerivedSubscriptReturnsRef()
    expectEqual(derived[0], 0)
    derived[0] = 42
    expectEqual(derived[0], 42)
}

FieldsTestSuite.test("Non-const subscript from derived returning ref") {
    var derived = DerivedNonConstSubscriptReturnsRef()
    expectEqual(derived[1], 0)
    derived[1] = -11
    expectEqual(derived[1], -11)
}

runAllTests()
