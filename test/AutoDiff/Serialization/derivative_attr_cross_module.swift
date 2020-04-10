// REQUIRES: asserts

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file.swiftmodule -primary-file %s %S/Inputs/derivative_attr_cross_module.swift
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file-2.swiftmodule %s -primary-file %S/Inputs/derivative_attr_cross_module.swift

// Check that merge-modules does not crash regardless of the order of inputs.
// RUN: %target-swift-frontend -emit-module -module-name Multi %t/multi-file.swiftmodule %t/multi-file-2.swiftmodule -o %t -print-stats 2>&1
// RUN: %target-swift-frontend -emit-module -module-name Multi %t/multi-file-2.swiftmodule %t/multi-file.swiftmodule -o %t -print-stats 2>&1

func id(_ x: Float) -> Float { x }
