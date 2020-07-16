// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xcc -std=c++17)
//
// REQUIRES: executable_test

import DeclWithDefinitionIncludingMembers
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("fully-defined") {
  let magicNumber = MagicNumber()
  var wrappedMagicNumber = FullyDefinedWrappedMagicNumber(t: magicNumber)
  expectEqual(wrappedMagicNumber.callGetInt(), 53)
}

runAllTests()
