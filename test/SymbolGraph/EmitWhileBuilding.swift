// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name EmitWhileBuilding -emit-module -emit-module-path %t/ -emit-symbol-graph -emit-symbol-graph-dir %t/
// RUN: %FileCheck %s --input-file %t/EmitWhileBuilding.symbols.json

/// Does a foo.
public func foo() {}

// CHECK: "precise":"s:17EmitWhileBuilding3fooyyF"
