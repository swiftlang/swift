// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -primary-file %S/Inputs/call_as_function_other_module.swift -emit-module-path %t/call_as_function_other_module.swiftmodule
// RUN: %target-swift-frontend -typecheck -I %t -primary-file %s -verify

import call_as_function_other_module

func testLayer<L: Layer>(_ layer: L) -> Float {
  return layer(1)
}

func testDense() -> Float {
  let dense = Dense()
  return dense(1)
}
