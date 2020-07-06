// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name MultipleAttributes -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name MultipleAttributes -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/MultipleAttributes.symbols.json

@frozen @propertyWrapper
public struct S {
  public init() {}
  public var wrappedValue: Int {
    return 0
  }
}

// CHECK-LABEL: "precise": "s:18MultipleAttributes1SV"
// CHECK: "declarationFragments": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "attribute",
// CHECK-NEXT:     "spelling": "@frozen"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": " "
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "attribute",
// CHECK-NEXT:     "spelling": "@propertyWrapper"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": " "
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "keyword",
// CHECK-NEXT:     "spelling": "struct"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": " "
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "identifier",
// CHECK-NEXT:     "spelling": "S"
// CHECK-NEXT:   }
// CHECK-NEXT: ]
