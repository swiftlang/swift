// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name TypeMethod -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name TypeMethod -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/TypeMethod.symbols.json

public struct S {
  // CHECK: "identifier": "swift.type.method"
  // CHECK-NEXT: "displayName": "Type Method"
  // CHECK: pathComponents
  // CHECK-NEXT: "S"
  // CHECK-NEXT: "bar()"
  public static func bar() {}
}
