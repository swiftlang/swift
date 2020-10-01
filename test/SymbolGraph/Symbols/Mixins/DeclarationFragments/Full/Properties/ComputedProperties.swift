// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ComputedProperties -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name ComputedProperties -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/ComputedProperties.symbols.json --check-prefix=XFULL
// RUN: %FileCheck %s --input-file %t/ComputedProperties.symbols.json --check-prefix=XSUBHEADING
// RUN: %FileCheck %s --input-file %t/ComputedProperties.symbols.json --check-prefix=YFULL
// RUN: %FileCheck %s --input-file %t/ComputedProperties.symbols.json --check-prefix=YSUBHEADING

public struct S {
  // We should show { get set } here for the
  // full declaration, but not for subheadings.
  public var x: Int {
    get { return 7 }
    set {}
  }
  public private(set) var y: Int {
    get { return 7 }
    set {}
  }
}

// XFULL-LABEL: "precise": "s:18ComputedProperties1SV1xSivp"
// XFULL: "declarationFragments": [
// XFULL-NEXT:   {
// XFULL-NEXT:     "kind": "keyword",
// XFULL-NEXT:     "spelling": "var"
// XFULL-NEXT:   },
// XFULL-NEXT:   {
// XFULL-NEXT:     "kind": "text",
// XFULL-NEXT:     "spelling": " "
// XFULL-NEXT:   },
// XFULL-NEXT:   {
// XFULL-NEXT:     "kind": "identifier",
// XFULL-NEXT:     "spelling": "x"
// XFULL-NEXT:   },
// XFULL-NEXT:   {
// XFULL-NEXT:     "kind": "text",
// XFULL-NEXT:     "spelling": ": "
// XFULL-NEXT:   },
// XFULL-NEXT:   {
// XFULL-NEXT:     "kind": "typeIdentifier",
// XFULL-NEXT:     "spelling": "Int",
// XFULL-NEXT:     "preciseIdentifier": "s:Si"
// XFULL-NEXT:   },
// XFULL-NEXT:   {
// XFULL-NEXT:     "kind": "text",
// XFULL-NEXT:     "spelling": " { "
// XFULL-NEXT:   },
// XFULL-NEXT:   {
// XFULL-NEXT:     "kind": "keyword",
// XFULL-NEXT:     "spelling": "get"
// XFULL-NEXT:   },
// XFULL-NEXT:   {
// XFULL-NEXT:     "kind": "text",
// XFULL-NEXT:     "spelling": " "
// XFULL-NEXT:   },
// XFULL-NEXT:   {
// XFULL-NEXT:     "kind": "keyword",
// XFULL-NEXT:     "spelling": "set"
// XFULL-NEXT:   },
// XFULL-NEXT:   {
// XFULL-NEXT:     "kind": "text",
// XFULL-NEXT:     "spelling": " }"
// XFULL-NEXT:   }
// XFULL-NEXT: ],

// XSUBHEADING-LABEL: "precise": "s:18ComputedProperties1SV1xSivp"
// XSUBHEADING: names
// XSUBHEADING: "subHeading": [
// XSUBHEADING-NEXT:   {
// XSUBHEADING-NEXT:     "kind": "keyword",
// XSUBHEADING-NEXT:     "spelling": "var"
// XSUBHEADING-NEXT:   },
// XSUBHEADING-NEXT:   {
// XSUBHEADING-NEXT:     "kind": "text",
// XSUBHEADING-NEXT:     "spelling": " "
// XSUBHEADING-NEXT:   },
// XSUBHEADING-NEXT:   {
// XSUBHEADING-NEXT:     "kind": "identifier",
// XSUBHEADING-NEXT:     "spelling": "x"
// XSUBHEADING-NEXT:   },
// XSUBHEADING-NEXT:   {
// XSUBHEADING-NEXT:     "kind": "text",
// XSUBHEADING-NEXT:     "spelling": ": "
// XSUBHEADING-NEXT:   },
// XSUBHEADING-NEXT:   {
// XSUBHEADING-NEXT:     "kind": "typeIdentifier",
// XSUBHEADING-NEXT:     "spelling": "Int",
// XSUBHEADING-NEXT:     "preciseIdentifier": "s:Si"
// XSUBHEADING-NEXT:   }
// XSUBHEADING-NEXT: ]

// YFULL-LABEL: "precise": "s:18ComputedProperties1SV1ySivp"
// YFULL: "declarationFragments": [
// YFULL-NEXT:   {
// YFULL-NEXT:     "kind": "keyword",
// YFULL-NEXT:     "spelling": "var"
// YFULL-NEXT:   },
// YFULL-NEXT:   {
// YFULL-NEXT:     "kind": "text",
// YFULL-NEXT:     "spelling": " "
// YFULL-NEXT:   },
// YFULL-NEXT:   {
// YFULL-NEXT:     "kind": "identifier",
// YFULL-NEXT:     "spelling": "y"
// YFULL-NEXT:   },
// YFULL-NEXT:   {
// YFULL-NEXT:     "kind": "text",
// YFULL-NEXT:     "spelling": ": "
// YFULL-NEXT:   },
// YFULL-NEXT:   {
// YFULL-NEXT:     "kind": "typeIdentifier",
// YFULL-NEXT:     "spelling": "Int",
// YFULL-NEXT:     "preciseIdentifier": "s:Si"
// YFULL-NEXT:   },
// YFULL-NEXT:   {
// YFULL-NEXT:     "kind": "text",
// YFULL-NEXT:     "spelling": " { "
// YFULL-NEXT:   },
// YFULL-NEXT:   {
// YFULL-NEXT:     "kind": "keyword",
// YFULL-NEXT:     "spelling": "get"
// YFULL-NEXT:   },
// YFULL-NEXT:   {
// YFULL-NEXT:     "kind": "text",
// YFULL-NEXT:     "spelling": " "
// YFULL-NEXT:   },
// YFULL-NEXT:   {
// YFULL-NEXT:     "kind": "keyword",
// YFULL-NEXT:     "spelling": "set"
// YFULL-NEXT:   },
// YFULL-NEXT:   {
// YFULL-NEXT:     "kind": "text",
// YFULL-NEXT:     "spelling": " }"
// YFULL-NEXT:   }
// YFULL-NEXT: ]

// YSUBHEADING-LABEL: "precise": "s:18ComputedProperties1SV1ySivp"
// YSUBHEADING: names
// YSUBHEADING: "subHeading": [
// YSUBHEADING-NEXT:   {
// YSUBHEADING-NEXT:     "kind": "keyword",
// YSUBHEADING-NEXT:     "spelling": "var"
// YSUBHEADING-NEXT:   },
// YSUBHEADING-NEXT:   {
// YSUBHEADING-NEXT:     "kind": "text",
// YSUBHEADING-NEXT:     "spelling": " "
// YSUBHEADING-NEXT:   },
// YSUBHEADING-NEXT:   {
// YSUBHEADING-NEXT:     "kind": "identifier",
// YSUBHEADING-NEXT:     "spelling": "y"
// YSUBHEADING-NEXT:   },
// YSUBHEADING-NEXT:   {
// YSUBHEADING-NEXT:     "kind": "text",
// YSUBHEADING-NEXT:     "spelling": ": "
// YSUBHEADING-NEXT:   },
// YSUBHEADING-NEXT:   {
// YSUBHEADING-NEXT:     "kind": "typeIdentifier",
// YSUBHEADING-NEXT:     "spelling": "Int",
// YSUBHEADING-NEXT:     "preciseIdentifier": "s:Si"
// YSUBHEADING-NEXT:   }
// YSUBHEADING-NEXT: ]
