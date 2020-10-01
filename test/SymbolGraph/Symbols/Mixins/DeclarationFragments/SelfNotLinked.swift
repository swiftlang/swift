// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SelfNotLinked -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SelfNotLinked -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SelfNotLinked@Swift.symbols.json --match-full-lines --strict-whitespace

extension Sequence where Self : Collection {
  public func foo(x: Self) {}
}

// CHECK-LABEL:        "precise": "s:ST13SelfNotLinkedSlRzrlE3foo1xyx_tF",
// CHECK:      "declarationFragments": [
// CHECK-NEXT:        {
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
// CHECK-NEXT:          "spelling": "("
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "externalParam",
// CHECK-NEXT:          "spelling": "x"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": ": "
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "typeIdentifier",
// CHECK-NEXT:          "spelling": "Self"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": ")"
// CHECK-NEXT:        }
// CHECK-NEXT:      ],
// CHECK-NEXT:      "accessLevel": "public"
// CHECK-NEXT:    }
// CHECK-NEXT:  ],
