// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import DeclWithPrimitiveArgument
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("int-argument") {
  var wrappedMagicInt = WrappedMagicInt(t: 42)
  expectEqual(wrappedMagicInt.getValuePlusArg(5), 47)
}

runAllTests()
