// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import DeclWithDefinition
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("has-partial-definition") {
  let myInt = IntWrapper(value: 32)
  var magicInt = PartiallyDefinedMagicallyWrappedInt(t: myInt)
  expectEqual(magicInt.getValuePlusArg(5), 37)
}

runAllTests()
