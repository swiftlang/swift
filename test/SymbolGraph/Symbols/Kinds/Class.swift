// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Class -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Class -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Class.symbols.json

// CHECK: "identifier": "swift.class"
// CHECK-NEXT: "displayName": "Class"
// CHECK: pathComponents
// CHECK-NEXT: "C"
public class C {}
