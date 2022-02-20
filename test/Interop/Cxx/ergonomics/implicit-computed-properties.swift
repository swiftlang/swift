// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import ImplicitComputedProperties

var ImplicitComputedPropertiesTestSuite = TestSuite("ImplicitComputedProperties")

ImplicitComputedPropertiesTestSuite.test("LongNameAllLower") {
    var VoidGetter = LongNameAllLower()

    expectEqual(VoidGetter.foo, 42)

}
ImplicitComputedPropertiesTestSuite.test("LongNameAllUpper") {
    var VoidGetter = LongNameAllUpper()

    expectEqual(VoidGetter.foo, 42)

}
ImplicitComputedPropertiesTestSuite.test("LongNameMix") {
    var VoidGetter = LongNameMix()

    expectEqual(VoidGetter.foo, 42)

}
ImplicitComputedPropertiesTestSuite.test("GetterOnly") {
    var VoidGetter = GetterOnly()
    expectEqual(VoidGetter.foo, 42)

}

ImplicitComputedPropertiesTestSuite.test("UTF8Str") {
    var VoidGetter  = UTF8Str()
    expectEqual(VoidGetter.utf8Str, 42)

}
ImplicitComputedPropertiesTestSuite.test("setters") {
    var Object = IntGetterSetter()
    Object.x = 1
    expectEqual(Object.x, 1)
}

// TODO: Omar put the radar link here.
//ImplicitComputedPropertiesTestSuite.test("ref") {
//    var Object = RefGetterSetter()
//    var i: CInt = 2
//    withUnsafePointer(to: &i) { Object.x = $0 }
//    expectEqual(Object.x.pointee, 2)
//}

ImplicitComputedPropertiesTestSuite.test("ptr") {
    var Object = PtrGetterSetter()
    var i: CInt = 2
    withUnsafeMutablePointer(to: &i) { Object.x! = $0 }
    expectEqual(Object.x!.pointee, 2)
}

ImplicitComputedPropertiesTestSuite.test("non trivial") {
    var Object = NonTrivialGetterSetter()
    Object.x = NonTrivial(value: 20)
    expectEqual(Object.x.value, 20)
}

runAllTests()