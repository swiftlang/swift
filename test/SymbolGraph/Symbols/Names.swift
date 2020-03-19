// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Names -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Names -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Names.symbols.json

public struct MyStruct {}

// CHECK: names
// CHECK-NEXT: "title": "MyStruct"
