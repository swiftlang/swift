// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(cross_module_differentiation_other)) %S/Inputs/cross_module_differentiation_other.swift -emit-module -emit-module-path %t/cross_module_differentiation_other.swiftmodule -module-name cross_module_differentiation_other
// RUN: %target-build-swift -I%t -L%t %s -o %t/a.out -lcross_module_differentiation_other %target-rpath(%t)
// RUN: %target-codesign %t/a.out
// RUN: %target-codesign %t/%target-library-name(cross_module_differentiation_other)
// RUN: %target-run %t/a.out %t/%target-library-name(cross_module_differentiation_other)
// REQUIRES: executable_test

// TF-1025: Test differentiability witness linkage for `PublicNonABI` original functions.
// TF-1239: Test `SynthesizedFileUnit` TBDGen.

import cross_module_differentiation_other
import _Differentiation
import StdlibUnittest

var CrossModuleTests = TestSuite("E2ECrossModule")

CrossModuleTests.test("differentiable function default argument") {
  let actualGrad = gradient(at: 0) { applyArgument($0) }
  let expectedGrad: Float = 1
  expectEqual(actualGrad, expectedGrad)
}

CrossModuleTests.test("differentiable function specified default argument") {
  let actualGrad = gradient(at: 0) { applyArgument($0, { $0 }) }
  let expectedGrad: Float = 1
  expectEqual(actualGrad, expectedGrad)
}

runAllTests()
