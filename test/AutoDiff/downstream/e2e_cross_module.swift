// RUN: %empty-directory(%t)
// RUN: %target-build-swift -working-directory %t -parse-as-library -emit-module -module-name e2e_cross_module_external_module -emit-module-path %t/e2e_cross_module_external_module.swiftmodule -emit-library -static %S/Inputs/e2e_cross_module_external_module.swift
// RUN: %target-build-swift -I%t -L%t %s -o %t/a.out -lm -le2e_cross_module_external_module
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import e2e_cross_module_external_module
import StdlibUnittest
import DifferentiationUnittest

var Tests = TestSuite("E2ECrossModule")

// Reproduces TF-1025.
Tests.testWithLeakChecking("differentiable function default argument") {
  let actualGrad = gradient(at: 0) { doubleThenApply($0) }
  let expectedGrad = Tracked<Float>(2)
  expectEqual(actualGrad, expectedGrad)
}

Tests.testWithLeakChecking("differentiable function specified default argument") {
  let actualGrad = gradient(at: 0) { doubleThenApply($0, { 10 * $0 }) }
  let expectedGrad = Tracked<Float>(20)
  expectEqual(actualGrad, expectedGrad)
}

runAllTests()
