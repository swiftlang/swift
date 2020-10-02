// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name ProtocolRequirements -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name ProtocolRequirements -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/ProtocolRequirements.symbols.json --check-prefix=FULL
// RUN: %FileCheck %s --input-file %t/ProtocolRequirements.symbols.json --check-prefix=SUBHEADING

public protocol P {
  // We should show { get set } here for the
  // full declaration, but not for subheadings.
  var x: Int { get set }
}

// FULL-LABEL: "precise": "s:20ProtocolRequirements1PP1xSivp"
// FULL: "declarationFragments": [
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "keyword",
// FULL-NEXT:     "spelling": "var"
// FULL-NEXT:   },
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "text",
// FULL-NEXT:     "spelling": " "
// FULL-NEXT:   },
// FULL-NEXT:   {
// FULL-NEXT:     "kind": "identifier",
// FULL-NEXT:     "spelling": "x"
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

// SUBHEADING-LABEL: "precise": "s:20ProtocolRequirements1PP1xSivp"
// SUBHEADING: names
// SUBHEADING: "subHeading": [
// SUBHEADING-NEXT:   {
// SUBHEADING-NEXT:     "kind": "keyword",
// SUBHEADING-NEXT:     "spelling": "var"
// SUBHEADING-NEXT:   },
// SUBHEADING-NEXT:   {
// SUBHEADING-NEXT:     "kind": "text",
// SUBHEADING-NEXT:     "spelling": " "
// SUBHEADING-NEXT:   },
// SUBHEADING-NEXT:   {
// SUBHEADING-NEXT:     "kind": "identifier",
// SUBHEADING-NEXT:     "spelling": "x"
// SUBHEADING-NEXT:   },
// SUBHEADING-NEXT:   {
// SUBHEADING-NEXT:     "kind": "text",
// SUBHEADING-NEXT:     "spelling": ": "
// SUBHEADING-NEXT:   },
// SUBHEADING-NEXT:   {
// SUBHEADING-NEXT:     "kind": "typeIdentifier",
// SUBHEADING-NEXT:     "spelling": "Int",
// SUBHEADING-NEXT:     "preciseIdentifier": "s:Si"
// SUBHEADING-NEXT:   }
// SUBHEADING-NEXT: ]
