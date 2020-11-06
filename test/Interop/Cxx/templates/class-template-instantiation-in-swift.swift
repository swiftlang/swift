// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import MagicWrapper
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("with-c++-type") {
  let intWrapper = IntWrapper(value: 42)
  var wrappedMagicNumber = MagicWrapper<IntWrapper>(t: intWrapper)
  expectEqual(wrappedMagicNumber.getValuePlusArg(8), 50)
}

TemplatesTestSuite.test("with-swift-type") {
  let _ = MagicWrapper<CInt>()
  var wrappedMagicNumber = MagicWrapper<CInt>(i: 13)
  expectEqual(wrappedMagicNumber.getValuePlusArg(8), 21)
}

runAllTests()
