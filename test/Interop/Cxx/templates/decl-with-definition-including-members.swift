// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import DeclWithDefinitionIncludingMembers
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("fully-defined") {
  let myInt = IntWrapper(value: 10)
  var magicInt = FullyDefinedMagicallyWrappedInt(t: myInt)
  expectEqual(magicInt.getValuePlusArg(5), 15)
}

runAllTests()
