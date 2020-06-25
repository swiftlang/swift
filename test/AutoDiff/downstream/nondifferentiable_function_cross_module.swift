// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -primary-file %S/Inputs/nondifferentiable_function_other_module.swift -emit-module-path %t/nondifferentiable_function_other_module.swiftmodule
// RUN: %target-swift-frontend -emit-sil -I %t -primary-file %s -verify

import nondifferentiable_function_other_module

func test() {
  // expected-error @+2 {{function is not differentiable}}
  // expected-note @+1 {{cannot differentiate functions that have not been marked '@differentiable' and that are defined in other files}}
  _ = gradient(at: Float(1), in: externalFunction)
}
