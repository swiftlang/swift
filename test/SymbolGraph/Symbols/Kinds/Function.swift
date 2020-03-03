// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Function -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Function -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Function.symbols.json

// CHECK: "identifier": "swift.func"
// CHECK-NEXT: "displayName": "Function"
// CHECK: pathComponents
// CHECK-NEXT: "foo()"
public func foo() {}
