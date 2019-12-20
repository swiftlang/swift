// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -parse-as-library -emit-module -module-name e2e_cross_module_external_module -emit-module-path %t/e2e_cross_module_external_module.swiftmodule -o %t/e2e_cross_module_external_module.o %S/Inputs/e2e_cross_module_external_module.swift
// RUN: %target-build-swift -I%t %s %t/e2e_cross_module_external_module.o -o %t/a.out -lm
// RUN: %target-run %t/a.out

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
