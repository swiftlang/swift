// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xcc -std=c++17)
//
// REQUIRES: executable_test

import TemplateWithPrimitiveArgument
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("has-int-argument") {
  var t = HasPrimitiveArgument(t: 42)
  expectEqual(t.callMethod(), 47)
}

runAllTests()
