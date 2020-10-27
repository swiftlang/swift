// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import ClassTemplateTemplateParameter
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("template-template-parameter") {
  let myInt = IntWrapper(value: 42)
  var magicInt = WrappedMagicInt(t: myInt)
  var templatedWrappedMagicInt = TemplatedWrappedMagicInt(i: magicInt)
  expectEqual(templatedWrappedMagicInt.getValuePlusTwiceTheArg(10), 62)
}

runAllTests()
