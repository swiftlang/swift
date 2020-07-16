// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xcc -std=c++17)
//
// REQUIRES: executable_test

import DeclWithoutDefinition
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("without-definition") {
  let magicNumber = MagicNumber()
  var wrappedMagicNumber = WrappedMagicNumberWithoutDefinition(t: magicNumber)
  expectEqual(wrappedMagicNumber.callGetInt(), 17)
}

runAllTests()
