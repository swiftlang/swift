// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xcc -std=c++17)
//
// REQUIRES: executable_test

import DeclWithoutDefinition
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("has-definition") {
  let a = Arg()
  var t = DoesNotHaveDefinition(t: a)
  expectEqual(t.callMethod(), 17)
}

runAllTests()
