// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Subscripts -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Subscripts -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Subscripts.symbols.json --check-prefix=FULL
// RUN: %FileCheck %s --input-file %t/Subscripts.symbols.json --check-prefix=SUBHEADING

public struct S {
  public subscript(i: Int) -> Int {
    get {
      return 7
    }
    set {}
  }
}

// FULL-LABEL: "precise": "s:10Subscripts1SVyS2icip"
// FULL: "functionSignature": {
// FULL: "returns": [
// FULL: "declarationFragments": [
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "keyword",
// FULL-NEXT:     "spelling": "subscript"
// FULL-NEXT:   },
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "text",
// FULL-NEXT:     "spelling": "("
// FULL-NEXT:   },
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "internalParam",
// FULL-NEXT:     "spelling": "i"
// FULL-NEXT:   },
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "text",
// FULL-NEXT:     "spelling": ": "
// FULL-NEXT:   },
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "typeIdentifier",
// FULL-NEXT:     "spelling": "Int",
// FULL-NEXT:     "preciseIdentifier": "s:Si"
// FULL-NEXT:   },
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "text",
// FULL-NEXT:     "spelling": ") -> "
// FULL-NEXT:   },
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "typeIdentifier",
// FULL-NEXT:     "spelling": "Int",
// FULL-NEXT:     "preciseIdentifier": "s:Si"
// FULL-NEXT:   },
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "text",
// FULL-NEXT:     "spelling": " { "
// FULL-NEXT:   },
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "keyword",
// FULL-NEXT:     "spelling": "get"
// FULL-NEXT:   },
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "text",
// FULL-NEXT:     "spelling": " "
// FULL-NEXT:   },
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "keyword",
// FULL-NEXT:     "spelling": "set"
// FULL-NEXT:   },
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "text",
// FULL-NEXT:     "spelling": " }"
// FULL-NEXT:   }
// FULL-NEXT: ]

// SUBHEADING-LABEL: "precise": "s:10Subscripts1SVyS2icip"
// SUBHEADING: names
// SUBHEADING: "subHeading": [
// SUBHEADING-NEXT:   {
// SUBHEADING-NEXT:     "kind": "keyword",
// SUBHEADING-NEXT:     "spelling": "subscript"
// SUBHEADING-NEXT:   },
// SUBHEADING-NEXT:   {
// SUBHEADING-NEXT:     "kind": "text",
// SUBHEADING-NEXT:     "spelling": "("
// SUBHEADING-NEXT:   },
// SUBHEADING-NEXT:   {
// SUBHEADING-NEXT:     "kind": "typeIdentifier",
// SUBHEADING-NEXT:     "spelling": "Int",
// SUBHEADING-NEXT:     "preciseIdentifier": "s:Si"
// SUBHEADING-NEXT:   },
// SUBHEADING-NEXT:   {
// SUBHEADING-NEXT:     "kind": "text",
// SUBHEADING-NEXT:     "spelling": ") -> "
// SUBHEADING-NEXT:   },
// SUBHEADING-NEXT:   {
// SUBHEADING-NEXT:     "kind": "typeIdentifier",
// SUBHEADING-NEXT:     "spelling": "Int",
// SUBHEADING-NEXT:     "preciseIdentifier": "s:Si"
// SUBHEADING-NEXT:   }
// SUBHEADING-NEXT: ]
