// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import UsingDirective

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("using-directive") {
  let myInt = IntWrapper(value: 333)
  var magicInt = UsingWrappedMagicNumber(t: myInt)
  expectEqual(magicInt.getValuePlusArg(111), 444)
}

runAllTests()
