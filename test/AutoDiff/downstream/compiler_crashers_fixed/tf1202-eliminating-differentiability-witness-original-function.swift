// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module -module-name tf1202 -emit-module-path %t/tf1202.swiftmodule %S/Inputs/tf1202-eliminating-differentiability-witness-original-function.swift
// RUN: %target-build-swift -I%t -emit-module -O %s

// This situation exposed a bug where DeadFunctionElimination eliminated the
// SILFunction for `identity<T>` even though a differentiability witness for it
// still existed. This causes deserialization of this module to crash when
// trying to deserialize the differentiability witness because it can't find
// the original function `identity<T>`.

import tf1202

func callit() -> Float {
    return foo()
}
