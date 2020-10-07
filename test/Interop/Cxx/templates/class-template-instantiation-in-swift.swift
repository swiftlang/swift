// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import UsingDirective
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("without-definition") {
  var intWrapper = IntWrapper(value: 42)
  var wrappedMagicNumber = MagicWrapper<IntWrapper>(t: intWrapper)
  expectEqual(wrappedMagicNumber.getValuePlusArg(8), 50)
}

runAllTests()
