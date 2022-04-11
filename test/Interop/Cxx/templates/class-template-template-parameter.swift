// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import ClassTemplateTemplateParameter
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("typedeffed-template-template-parameter") {
  let myInt = IntWrapper(value: 42)
  var magicInt = WrappedMagicInt(t: myInt)
  var templatedWrappedMagicInt = TemplatedWrappedMagicInt(i: magicInt)
  expectEqual(templatedWrappedMagicInt.getValuePlusTwiceTheArg(10), 62)
}

// TODO(SR-13261): test below doesn't work because Swift needs concrete generic
// arguments for MagicWrapper.
// TemplatesTestSuite.test("template-template-parameter") {
//   let myInt = IntWrapper(value: 42)
//   var magicInt = MagicWrapper<IntWrapper>(t: myInt)
//   var templatedWrappedMagicInt = TemplatedMagicWrapper<MagicWrapper>(i: magicInt)
//   expectEqual(templatedWrappedMagicInt.getValuePlusTwiceTheArg(10), 62)
// }

runAllTests()
