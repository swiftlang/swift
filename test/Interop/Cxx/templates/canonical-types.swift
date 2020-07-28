// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop -Xcc -std=c++17)
//
// REQUIRES: executable_test

import CanonicalTypes
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("canonical-types") {
  // multiple typedeffed types with the same canonical type are the same type
  // from the typechecking perspective.
  expectEqualType(WrappedMagicNumberA.self, WrappedMagicNumberB.self)
}

runAllTests()
