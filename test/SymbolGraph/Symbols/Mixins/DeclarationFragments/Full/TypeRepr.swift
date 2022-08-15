// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name TypeRepr -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name TypeRepr -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/TypeRepr.symbols.json

// prior to being added to the printer impl, `_const` parameters were rendered as `inout` in
// non-assertion builds. ensure it renders correctly.

public func asdf(param: _const String? = nil) {}

// CHECK-LABEL: "precise": "s:8TypeRepr4asdf5paramySSSgYt_tF"

// the `declarationFragments` token is part of the `functionSignature` first, so skip that
// CHECK: "functionSignature"
// CHECK: "declarationFragments"

// CHECK:      "declarationFragments": [
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
// CHECK-NEXT:     "spelling": "asdf"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": "("
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "externalParam",
// CHECK-NEXT:     "spelling": "param"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": ": "
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "typeIdentifier",
// CHECK-NEXT:     "spelling": "String",
// CHECK-NEXT:     "preciseIdentifier": "s:SS"
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "kind": "text",
// CHECK-NEXT:     "spelling": "? = nil)"
// CHECK-NEXT:   }
// CHECK-NEXT: ],
