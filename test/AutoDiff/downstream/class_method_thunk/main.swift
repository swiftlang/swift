// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/../Inputs/class_method_thunk_other_module.swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test

import StdlibUnittest

var ClassMethodThunkTests = TestSuite("ClassMethodThunks")

func classValueWithGradient(_ c: OtherModuleSuper) -> (Float, Float) {
  return valueWithGradient(at: 1) { c.f($0) }
}

ClassMethodThunkTests.test("CrossModuleClassMethodThunks") {
  expectEqual((2, 2), classValueWithGradient(OtherModuleSuper()))
  expectEqual((3, 3), classValueWithGradient(OtherModuleSubOverride()))
  expectEqual((3, 3), classValueWithGradient(OtherModuleSubOverrideCustomDerivatives()))
}

runAllTests()
