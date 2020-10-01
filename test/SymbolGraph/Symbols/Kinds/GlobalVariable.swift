// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name GlobalVariable -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name GlobalVariable -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/GlobalVariable.symbols.json

// CHECK: "identifier": "swift.var"
// CHECK-NEXT: "displayName": "Global Variable"
// CHECK: pathComponents
// CHECK-NEXT: "global"
public let global = 2
