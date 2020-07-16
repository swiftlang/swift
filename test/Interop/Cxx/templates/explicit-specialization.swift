// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xcc -std=c++17)
//
// REQUIRES: executable_test

import ExplicitSpecialization
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("explicit-specialization") {
  let magicNumber = MagicNumber()
  var wrappedMagicNumber = MagicWrappedNumberWithExplicitSpecialization(t: magicNumber)
  expectEqual(wrappedMagicNumber.callGetInt(), 36)
}

runAllTests()
