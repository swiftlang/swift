// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name PrimaryAssocType -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name PrimaryAssocType -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/PrimaryAssocType.symbols.json

public protocol WithPrimary<Assoc> {
    associatedtype Assoc
}

// CHECK-LABEL: "precise": "s:16PrimaryAssocType04WithA0P"
// CHECK:           "declarationFragments": [
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "keyword"
// CHECK-NEXT:          "spelling": "protocol"
// CHECK-NEXT:        }
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text"
// CHECK-NEXT:          "spelling": " "
// CHECK-NEXT:        }
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "identifier"
// CHECK-NEXT:          "spelling": "WithPrimary"
// CHECK-NEXT:        }
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text"
// CHECK-NEXT:          "spelling": "<"
// CHECK-NEXT:        }
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "typeIdentifier"
// CHECK-NEXT:          "spelling": "Assoc"
// CHECK-NEXT:          "preciseIdentifier": "s:16PrimaryAssocType04WithA0P0B0Qa"
// CHECK-NEXT:        }
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text"
// CHECK-NEXT:          "spelling": ">"
// CHECK-NEXT:        }
// CHECK-NEXT:      ]
