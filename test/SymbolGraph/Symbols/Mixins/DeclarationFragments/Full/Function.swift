// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Function -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Function -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Function.symbols.json --match-full-lines --strict-whitespace

public func foo<S>(f: @escaping () -> (), ext int: Int = 2, s: S) where S: Sequence {}

// CHECK-LABEL:{{^      }}"declarationFragments": [
// CHECK:        {
// CHECK-NEXT:          "kind": "keyword",
// CHECK-NEXT:          "spelling": "func"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": " "
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "identifier",
// CHECK-NEXT:          "spelling": "foo"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": "<"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "genericParameter",
// CHECK-NEXT:          "spelling": "S"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": ">("
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "externalParam",
// CHECK-NEXT:          "spelling": "f"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": ": "
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "keyword",
// CHECK-NEXT:          "spelling": "@escaping"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": " () -> (), "
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "externalParam",
// CHECK-NEXT:          "spelling": "ext"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": " "
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "internalParam",
// CHECK-NEXT:          "spelling": "int"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": ": "
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "typeIdentifier",
// CHECK-NEXT:          "spelling": "Int",
// CHECK-NEXT:          "preciseIdentifier": "s:Si"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": " = 2, "
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "externalParam",
// CHECK-NEXT:          "spelling": "s"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": ": S"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": ") "
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "keyword",
// CHECK-NEXT:          "spelling": "where"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": " S"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": " : "
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "typeIdentifier",
// CHECK-NEXT:          "spelling": "Sequence",
// CHECK-NEXT:          "preciseIdentifier": "s:ST"
// CHECK-NEXT:        }
// CHECK-NEXT:      ],
