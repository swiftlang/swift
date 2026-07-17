// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import ExplicitComputedPropertiesRuntime

var ExplicitComputedPropertiesRuntimeTestSuite = TestSuite("ExplicitComputedPropertiesRuntime")

ExplicitComputedPropertiesRuntimeTestSuite.test("GetterSetter") {
  var object = GetterSetter()
  expectEqual(object.get_x(), object.x)
  object.set_x(1)
  expectEqual(object.get_x(), 1)
  expectEqual(object.x, 1)
  object.x = 2
  expectEqual(object.get_x(), 2)
  expectEqual(object.x, 2)
}

ExplicitComputedPropertiesRuntimeTestSuite.test("GetterOnly") {
  let object = GetterOnly()
  expectEqual(object.get_value(), object.value)
}

ExplicitComputedPropertiesRuntimeTestSuite.test("SnakeCaseAcronym") {
  var object = SnakeCaseAcronym()
  expectEqual(object.get_http_URL(), object.httpURL)
  object.set_http_URL(7)
  expectEqual(object.get_http_URL(), 7)
  expectEqual(object.httpURL, 7)
}

ExplicitComputedPropertiesRuntimeTestSuite.test("NoPrefix") {
  let object = SnakeCaseNoPrefix()
  expectEqual(object.im_snake_case_swift_computed_property(),
              object.imSnakeCaseSwiftComputedProperty)
}

runAllTests()
