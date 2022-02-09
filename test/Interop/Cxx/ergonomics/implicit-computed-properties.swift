// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import ImplicitComputedProperties

var ImplicitComputedPropertiesTestSuite = TestSuite("ImplicitComputedProperties")

ImplicitComputedPropertiesTestSuite.test("LongNameAllLower") {
    var VoidGetter = LongNameAllLower()

    expectEqual(VoidGetter.get, 42)

}
ImplicitComputedPropertiesTestSuite.test("LongNameAllUpper") {
    var VoidGetter = LongNameAllUpper()

    expectEqual(VoidGetter.get, 42)

}
ImplicitComputedPropertiesTestSuite.test("LongNameMix") {
    var VoidGetter = LongNameMix()

    expectEqual(VoidGetter.get, 42)

}
ImplicitComputedPropertiesTestSuite.test("GetterOnly") {
    var VoidGetter = GetterOnly()

    expectEqual(VoidGetter.get, 42)

}

ImplicitComputedPropertiesTestSuite.test("setters") {
    var Object = X()
    Object.x = 1
    expectEqual(Object.x, 1)
}

runAllTests()