// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import CanonicalTypes
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("canonical-types") {
  // Different typedefs with the same C++ canonical type must have the same type from Swift's perspective as well.
  expectEqualType(WrappedMagicNumberA.self, WrappedMagicNumberB.self)
}

runAllTests()
