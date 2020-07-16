// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xcc -std=c++17)
//
// REQUIRES: executable_test

import DeclWithPrimitiveArgument
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("int-argument") {
  var wrappedMagicInt = WrappedMagicInt(t: 42)
  expectEqual(wrappedMagicInt.getInt(), 47)
}

runAllTests()
