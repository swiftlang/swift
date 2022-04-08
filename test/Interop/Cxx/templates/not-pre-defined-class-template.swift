// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import NotPreDefinedClassTemplate
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("typedef-without-definition") {
  let myInt = IntWrapper(value: 17)
  var magicInt = MagicallyWrappedIntWithoutDefinition(t: myInt)
  expectEqual(magicInt.getValuePlusArg(11), 28)
}

runAllTests()
