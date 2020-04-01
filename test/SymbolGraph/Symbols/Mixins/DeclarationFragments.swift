// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name DeclarationFragments -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name DeclarationFragments -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/DeclarationFragments.symbols.json

public func foo<S>(f: @escaping () -> (), x: Int = 2, s: S) {}

// CHECK: declarationFragments

// CHECK: "kind": "keyword",
// CHECK-NEXT: "spelling": "func"

// CHECK: "kind": "text",
// CHECK-NEXT: "spelling": " "

// CHECK: "kind": "identifier",
// CHECK-NEXT: "spelling": "foo"

// CHECK: "kind": "text",
// CHECK-NEXT: "spelling": "<"

// CHECK: "kind": "genericParameter",
// CHECK-NEXT: "spelling": "S"

// CHECK: "kind": "text",
// CHECK-NEXT: "spelling": ">("

// CHECK: "kind": "identifier",
// CHECK-NEXT: "spelling": "f"

// CHECK: "kind": "text",
// CHECK-NEXT: "spelling": ": () -> (), "

// CHECK: "kind": "identifier",
// CHECK-NEXT: "spelling": "x"

// CHECK: "kind": "text",
// CHECK-NEXT: "spelling": ": "

// CHECK: "kind": "typeIdentifier",
// CHECK-NEXT: "spelling": "Int",
// CHECK-NEXT: "preciseIdentifier": "s:Si"

// CHECK: "kind": "text",
// CHECK-NEXT: "spelling": " = 2, "

// CHECK: "kind": "identifier",
// CHECK-NEXT: "spelling": "s"

// CHECK: "kind": "text",
// CHECK-NEXT: "spelling": ": S"

// CHECK: "kind": "text",
// CHECK-NEXT: "spelling": ")"
