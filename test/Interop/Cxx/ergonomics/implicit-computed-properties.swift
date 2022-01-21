// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import ImplicitComputedProperties

var ImplicitComputedPropertiesTestSuite = TestSuite("ImplicitComputedProperties")

ImplicitComputedPropertiesTestSuite.test("getters") {

}

ImplicitComputedPropertiesTestSuite.test("setters") {
    var Object = X()
    Object.x = 1

    expectEqual(Object.x, 1)
}
runAllTests()