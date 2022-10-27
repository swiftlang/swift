// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xfrontend -disable-availability-checking)
//
// REQUIRES: executable_test

import ClassTemplateInstantiationErrors
import StdlibUnittest

var TemplatesTestSuite = TestSuite("Template with uninstantiatable members")

TemplatesTestSuite.test("Calls valid member") {
  var x = CannotBeInstantianted<IntWrapper>(IntWrapper(value: 41))
  expectEqual(x.incValue(), 42)
}

TemplatesTestSuite.test("Calls valid member on arg") {
  var x = CannotBeInstantianted<IntWrapper>(IntWrapper(value: 0))
  expectEqual(x.incValue(IntWrapper(value: 41)), 42)
}

runAllTests()
