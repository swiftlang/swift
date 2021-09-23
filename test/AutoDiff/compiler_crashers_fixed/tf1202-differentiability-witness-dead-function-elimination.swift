// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module -module-name tf1202 -emit-module-path %t/tf1202.swiftmodule %S/Inputs/tf1202-differentiability-witness-dead-function-elimination.swift
// RUN: %target-build-swift -I%t -emit-module -O %s

// TF-1202: test bug where DeadFunctionElimination eliminated the
// SILFunction for `func identity<T>` even though a differentiability witness for it
// exists. This causes deserialization of this module to crash when
// trying to deserialize the differentiability witness because it can't find
// the original function `func identity<T>`.

// TF-1239: Test `SynthesizedFileUnit` serialization.

import tf1202

func callit() -> Float {
  return foo()
}
