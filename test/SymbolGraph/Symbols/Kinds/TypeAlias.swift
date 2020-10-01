// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name TypeAlias -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name TypeAlias -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/TypeAlias.symbols.json

// CHECK: "identifier": "swift.typealias"
// CHECK-NEXT: "displayName": "Type Alias"
// CHECK: pathComponents
// CHECK-NEXT: "Alias"
public typealias Alias = Int
