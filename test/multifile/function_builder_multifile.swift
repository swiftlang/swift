// RUN: %target-swift-frontend -typecheck %S/Inputs/function_builder_definition.swift -primary-file %s

func test0() -> (Int, Double, String) {
  return tuplify {
    17
    3.14159
    "Hello"
  }
}
