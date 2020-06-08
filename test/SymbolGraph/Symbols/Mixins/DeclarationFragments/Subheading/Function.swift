// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Function -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Function -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Function.symbols.json

public func foo<S>(f: @escaping () -> (), ext int: Int = 2, s: S) where S: Sequence {}

// Subheading fragments should not contain internalParam kinds.

// CHECK-LABEL: "precise": "s:8Function3foo1f3ext1syyyc_SixtSTRzlF"
// CHECK: names
// CHECK-NEXT: "title": "foo(f:ext:s:)"
// CHECK:      "subHeading": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "keyword",
// CHECK-NEXT:     "spelling": "func"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": " "
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "identifier",
// CHECK-NEXT:     "spelling": "foo"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": "<"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "genericParameter",
// CHECK-NEXT:     "spelling": "S"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": ">("
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "externalParam",
// CHECK-NEXT:     "spelling": "f"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": ": () -> (), "
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "externalParam",
// CHECK-NEXT:     "spelling": "ext"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": ": "
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "typeIdentifier",
// CHECK-NEXT:     "spelling": "Int",
// CHECK-NEXT:     "preciseIdentifier": "s:Si"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": ", "
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "externalParam",
// CHECK-NEXT:     "spelling": "s"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": ": S"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": ")"
// CHECK-NEXT:   }
// CHECK-NEXT: ]

