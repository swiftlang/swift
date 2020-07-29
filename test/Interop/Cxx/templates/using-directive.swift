// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xcc -std=c++17)
//
// REQUIRES: executable_test

import UsingDirective
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("using-directive") {
  let myInt = IntWrapper(value: 333)
  var magicInt = UsingWrappedMagicNumber(t: myInt)
  expectEqual(magicInt.getValuePlusArg(111), 444)
}

runAllTests()
