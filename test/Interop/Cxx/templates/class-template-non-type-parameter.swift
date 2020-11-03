// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import ClassTemplateNonTypeParameter
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("non-type-parameter") {
  var pair = MagicIntPair(t: (1, 2))
  expectEqual(pair.t, (1, 2))
}

runAllTests()
