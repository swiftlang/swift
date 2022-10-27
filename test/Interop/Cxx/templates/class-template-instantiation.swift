// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -Xfrontend -disable-availability-checking)
//
// REQUIRES: executable_test

import ClassTemplateInstantiation
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("with-c++-type") {
  let intWrapper = IntWrapper(value: 42)
  var wrappedMagicNumber = MagicWrapper<IntWrapper>(t: intWrapper)
  expectEqual(wrappedMagicNumber.getValuePlusArg(8), 50)
}

TemplatesTestSuite.test("with-swift-type") {
  var wrappedMagicNumber = MagicWrapper<CInt>(i: 13)
  expectEqual(wrappedMagicNumber.getValuePlusArg(8), 21)
}

TemplatesTestSuite.test("with-c++-type-calling-method-on-arg")
  .skip(.watchOSSimulatorAny("rdar://problem/87262809")).code {
  let i1 = IntWrapper(value: 42)
  let i2 = IntWrapper(value: 12)
  var wrappedMagicNumber = MagicWrapper<IntWrapper>(t: i1)
  expectEqual(wrappedMagicNumber.getValuePlusArg(i2), 54)
}

runAllTests()
