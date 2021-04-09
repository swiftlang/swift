// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name BatchMode -emit-module-path %t/BatchMode.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t -O -enable-batch-mode -incremental
// RUN: %FileCheck %s --input-file %t/BatchMode.symbols.json

/// This is some func.
public func someFunc() {}

// CHECK: range
