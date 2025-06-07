// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name CustomAttr -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name CustomAttr -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/CustomAttr.symbols.json

@propertyWrapper
public struct SomeWrapper {
  public var wrappedValue: Int { 0 }
  public init(wrappedValue: Int) {}
}

public func wrapped(@SomeWrapper arg: Int) {}

// CHECK-LABEL: "precise": "s:10CustomAttr7wrapped3argySi_tF"
// CHECK: "declarationFragments": [
// CHECK: "declarationFragments": [
// CHECK-NEXT:  {
// CHECK-NEXT:    "kind": "keyword",
// CHECK-NEXT:    "spelling": "func"
// CHECK-NEXT:  },
// CHECK-NEXT:  {
// CHECK-NEXT:    "kind": "text",
// CHECK-NEXT:    "spelling": " "
// CHECK-NEXT:  },
// CHECK-NEXT:  {
// CHECK-NEXT:    "kind": "identifier",
// CHECK-NEXT:    "spelling": "wrapped"
// CHECK-NEXT:  },
// CHECK-NEXT:  {
// CHECK-NEXT:    "kind": "text",
// CHECK-NEXT:    "spelling": "("
// CHECK-NEXT:  },
// CHECK-NEXT:  {
// CHECK-NEXT:    "kind": "attribute",
// CHECK-NEXT:    "spelling": "@"
// CHECK-NEXT:  },
// CHECK-NEXT:  {
// CHECK-NEXT:    "kind": "attribute",
// CHECK-NEXT:    "spelling": "SomeWrapper",
// CHECK-NEXT:    "preciseIdentifier": "s:10CustomAttr11SomeWrapperV"
// CHECK-NEXT:  },
// CHECK-NEXT:  {
// CHECK-NEXT:    "kind": "text",
// CHECK-NEXT:    "spelling": " "
// CHECK-NEXT:  },
// CHECK-NEXT:  {
// CHECK-NEXT:    "kind": "externalParam",
// CHECK-NEXT:    "spelling": "arg"
// CHECK-NEXT:  },
// CHECK-NEXT:  {
// CHECK-NEXT:    "kind": "text",
// CHECK-NEXT:    "spelling": ": "
// CHECK-NEXT:  },
// CHECK-NEXT:  {
// CHECK-NEXT:    "kind": "typeIdentifier",
// CHECK-NEXT:    "spelling": "Int",
// CHECK-NEXT:    "preciseIdentifier": "s:Si"
// CHECK-NEXT:  },
// CHECK-NEXT:  {
// CHECK-NEXT:    "kind": "text",
// CHECK-NEXT:    "spelling": ")"
// CHECK-NEXT:  }
// CHECK-NEXT:]
