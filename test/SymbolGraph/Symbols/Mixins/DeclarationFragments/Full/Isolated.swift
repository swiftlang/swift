// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Isolated -emit-module -emit-module-path %t/Isolated.swiftmodule -target %target-swift-5.1-abi-triple
// RUN: %target-swift-symbolgraph-extract -module-name Isolated -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Isolated.symbols.json

// REQUIRES: concurrency

// prior to being added to the printer impl, `isolated` appeared as `inout` in non-assertion builds.
// ensure it renders correctly.

public actor SomeActor {}

public func asdf(param: isolated SomeActor) {}

// CHECK-LABEL:  "precise": "s:8Isolated4asdf5paramyAA9SomeActorCYi_tF",

// the `declarationFragments` token is part of the `functionSignature` first, so skip that
// CHECK: "functionSignature"
// CHECK: "declarationFragments"

// CHECK:           "declarationFragments": [
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
// CHECK-NEXT:          "spelling": "asdf"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": "("
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "externalParam",
// CHECK-NEXT:          "spelling": "param"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": ": "
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "keyword",
// CHECK-NEXT:          "spelling": "isolated"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": " "
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "typeIdentifier",
// CHECK-NEXT:          "spelling": "SomeActor",
// CHECK-NEXT:          "preciseIdentifier": "s:8Isolated9SomeActorC"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "kind": "text",
// CHECK-NEXT:          "spelling": ")"
// CHECK-NEXT:        }
// CHECK-NEXT:      ],
