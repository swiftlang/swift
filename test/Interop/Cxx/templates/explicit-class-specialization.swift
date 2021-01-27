// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import ExplicitClassSpecialization
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("explicit-specialization") {
  let specializedInt = SpecializedIntWrapper(value: 7)
  var specializedMagicInt = WrapperWithSpecialization(t: specializedInt)
  expectEqual(specializedMagicInt.doubleIfSpecializedElseTriple(), 14)

  let nonSpecializedInt = NonSpecializedIntWrapper(value: 7)
  var nonSpecializedMagicInt = WrapperWithoutSpecialization(t: nonSpecializedInt)
  expectEqual(nonSpecializedMagicInt.doubleIfSpecializedElseTriple(), 21)
}

runAllTests()
