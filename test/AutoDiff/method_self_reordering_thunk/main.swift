// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/../Inputs/method_self_reordering_thunk_other_module.swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test

import StdlibUnittest

var MethodSelfReorderingThunkTests = TestSuite("MethodSelfReorderingThunks")

// Test TF-619: cross-module import of `@differentiable` methods with
// self-ordering thunks.
MethodSelfReorderingThunkTests.test("CrossModuleMethodSelfReorderingThunk") {
  expectEqual(1, gradient(at: 0) { x in TF_619().foo(x) })
}

runAllTests()
