// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xcc -std=c++17)
//
// REQUIRES: executable_test

import UsingDirective
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("using-directive") {
  let arg = Arg()
  var tpl = ComesFromUsingDirective(t: arg)
  expectEqual(tpl.callMethod(), 11)
}

runAllTests()
