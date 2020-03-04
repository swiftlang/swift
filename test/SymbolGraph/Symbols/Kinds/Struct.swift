// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Struct -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Struct -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Struct.symbols.json

// CHECK: "identifier": "swift.struct"
// CHECK-NEXT: "displayName": "Structure"
// CHECK: pathComponents
// CHECK-NEXT: "S"
public struct S {}
