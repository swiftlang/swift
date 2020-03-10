// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name TypeSubscript -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name TypeSubscript -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/TypeSubscript.symbols.json

public struct S {
  // CHECK: "identifier": "swift.type.subscript"
  // CHECK-NEXT: "displayName": "Type Subscript"
  // CHECK: pathComponents
  // CHECK-NEXT: "S"
  // CHECK-NEXT: "subscript(_:)"
  public static subscript(i: Int) -> Int {
    return 2
  }
}
