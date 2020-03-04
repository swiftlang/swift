// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Enum -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Enum -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Enum.symbols.json

// CHECK: "identifier": "swift.enum"
// CHECK-NEXT: "displayName": "Enumeration"
// CHECK: pathComponents
// CHECK-NEXT: "E"
public enum E {}
