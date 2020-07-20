// REQUIRES: asserts
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file.swiftmodule -primary-file %s %S/Inputs/multi-file-type-eraser-other.swift
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file-2.swiftmodule %s -primary-file %S/Inputs/multi-file-type-eraser-other.swift

// RUN: %target-swift-frontend -emit-module -module-name Multi %t/multi-file.swiftmodule %t/multi-file-2.swiftmodule -o %t -print-stats 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-module -module-name Multi %t/multi-file-2.swiftmodule %t/multi-file.swiftmodule -o %t -print-stats 2>&1 | %FileCheck %s
// Ensure we don't crash during merge-modules - no matter the order of inputs.

// CHECK: Statistics
// CHECK: 2 Serialization - # of normal protocol conformances completed

public struct AnyWindows : Vista {
  public init<V: Vista>(erasing vista: V) {
    fatalError()
  }

  public var dlls: Never { fatalError() }
}

