// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xcc -std=c++17)
//
// REQUIRES: executable_test

import UsingDirective
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("using-directive") {
  let magicNumber = MagicNumber()
  var wrappedMagicNumber = UsingWrappedMagicNumber(t: magicNumber)
  expectEqual(wrappedMagicNumber.callGetInt(), 11)
}

runAllTests()
