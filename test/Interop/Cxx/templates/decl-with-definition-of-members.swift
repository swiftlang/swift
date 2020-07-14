// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xcc -std=c++17)
//
// REQUIRES: executable_test

import DeclWithDefinitionOfMembers
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("fully-instantiated") {
  let a = Arg()
  var t = DefinedMembers(t: a)
  expectEqual(t.callMethod(), 53)
}

runAllTests()
