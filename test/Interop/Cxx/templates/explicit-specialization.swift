// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xcc -std=c++17)
//
// REQUIRES: executable_test

import ExplicitSpecialization
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("explicit-specialization-is-used") {
  let arg = Arg()
  var tpl = TplWithExplicitSpecialization(t: arg)
  expectEqual(tpl.callMethod(), 36)
}

runAllTests()
