// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name BatchMode -emit-module-path %t/BatchMode.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t -O -enable-batch-mode
// RUN: %FileCheck %s --input-file %t/BatchMode.symbols.json

// changes to the doc comment cache created situations where building in batch mode caused symbol
// graphs to lose source range information for individual doc comment lines.

// CHECK: "range":{"start":{"line":[[# @LINE]],"character":4},"end":{"line":[[# @LINE]],"character":22}}
/// This is some func.
public func someFunc() {}
