// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name NominalTypes -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name NominalTypes -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/NominalTypes.symbols.json --check-prefix=STRUCT
// RUN: %FileCheck %s --input-file %t/NominalTypes.symbols.json --check-prefix=CLASS
// RUN: %FileCheck %s --input-file %t/NominalTypes.symbols.json --check-prefix=ENUM
// RUN: %FileCheck %s --input-file %t/NominalTypes.symbols.json --check-prefix=TYPEALIAS

public protocol P {}

@frozen public struct S<T> : P where T: Sequence {}

// STRUCT-LABEL: "precise": "s:12NominalTypes1SV",
// STRUCT: "declarationFragments": [
// STRUCT-NEXT:   {
// STRUCT-NEXT:     "kind": "attribute",
// STRUCT-NEXT:     "spelling": "@frozen"
// STRUCT-NEXT:   },
// STRUCT-NEXT:   {
// STRUCT-NEXT:     "kind": "text",
// STRUCT-NEXT:     "spelling": " "
// STRUCT-NEXT:   },
// STRUCT-NEXT:   {
// STRUCT-NEXT:     "kind": "keyword",
// STRUCT-NEXT:     "spelling": "struct"
// STRUCT-NEXT:   },
// STRUCT-NEXT:   {
// STRUCT-NEXT:     "kind": "text",
// STRUCT-NEXT:     "spelling": " "
// STRUCT-NEXT:   },
// STRUCT-NEXT:   {
// STRUCT-NEXT:     "kind": "identifier",
// STRUCT-NEXT:     "spelling": "S"
// STRUCT-NEXT:   },
// STRUCT-NEXT:   {
// STRUCT-NEXT:     "kind": "text",
// STRUCT-NEXT:     "spelling": "<"
// STRUCT-NEXT:   },
// STRUCT-NEXT:   {
// STRUCT-NEXT:     "kind": "genericParameter",
// STRUCT-NEXT:     "spelling": "T"
// STRUCT-NEXT:   },
// STRUCT-NEXT:   {
// STRUCT-NEXT:     "kind": "text",
// STRUCT-NEXT:     "spelling": "> "
// STRUCT-NEXT:   },
// STRUCT-NEXT:   {
// STRUCT-NEXT:     "kind": "keyword",
// STRUCT-NEXT:     "spelling": "where"
// STRUCT-NEXT:   },
// STRUCT-NEXT:   {
// STRUCT-NEXT:     "kind": "text",
// STRUCT-NEXT:     "spelling": " T"
// STRUCT-NEXT:   },
// STRUCT-NEXT:   {
// STRUCT-NEXT:     "kind": "text",
// STRUCT-NEXT:     "spelling": " : "
// STRUCT-NEXT:   },
// STRUCT-NEXT:   {
// STRUCT-NEXT:     "kind": "typeIdentifier",
// STRUCT-NEXT:     "spelling": "Sequence",
// STRUCT-NEXT:     "preciseIdentifier": "s:ST"
// STRUCT-NEXT:   }
// STRUCT-NEXT: ]

public class C<T> where T: Sequence {}

// CLASS-LABEL: "precise": "s:12NominalTypes1CC",
// CLASS: "declarationFragments": [
// CLASS-NEXT:   {
// CLASS-NEXT:     "kind": "keyword",
// CLASS-NEXT:     "spelling": "class"
// CLASS-NEXT:   },
// CLASS-NEXT:   {
// CLASS-NEXT:     "kind": "text",
// CLASS-NEXT:     "spelling": " "
// CLASS-NEXT:   },
// CLASS-NEXT:   {
// CLASS-NEXT:     "kind": "identifier",
// CLASS-NEXT:     "spelling": "C"
// CLASS-NEXT:   },
// CLASS-NEXT:   {
// CLASS-NEXT:     "kind": "text",
// CLASS-NEXT:     "spelling": "<"
// CLASS-NEXT:   },
// CLASS-NEXT:   {
// CLASS-NEXT:     "kind": "genericParameter",
// CLASS-NEXT:     "spelling": "T"
// CLASS-NEXT:   },
// CLASS-NEXT:   {
// CLASS-NEXT:     "kind": "text",
// CLASS-NEXT:     "spelling": "> "
// CLASS-NEXT:   },
// CLASS-NEXT:   {
// CLASS-NEXT:     "kind": "keyword",
// CLASS-NEXT:     "spelling": "where"
// CLASS-NEXT:   },
// CLASS-NEXT:   {
// CLASS-NEXT:     "kind": "text",
// CLASS-NEXT:     "spelling": " T"
// CLASS-NEXT:   },
// CLASS-NEXT:   {
// CLASS-NEXT:     "kind": "text",
// CLASS-NEXT:     "spelling": " : "
// CLASS-NEXT:   },
// CLASS-NEXT:   {
// CLASS-NEXT:     "kind": "typeIdentifier",
// CLASS-NEXT:     "spelling": "Sequence",
// CLASS-NEXT:     "preciseIdentifier": "s:ST"
// CLASS-NEXT:   }
// CLASS-NEXT: ],

public enum E<T> where T: Sequence {}

// ENUM-LABEL: "precise": "s:12NominalTypes1EO",
// ENUM: "declarationFragments": [
// ENUM-NEXT:   {
// ENUM-NEXT:     "kind": "keyword",
// ENUM-NEXT:     "spelling": "enum"
// ENUM-NEXT:   },
// ENUM-NEXT:   {
// ENUM-NEXT:     "kind": "text",
// ENUM-NEXT:     "spelling": " "
// ENUM-NEXT:   },
// ENUM-NEXT:   {
// ENUM-NEXT:     "kind": "identifier",
// ENUM-NEXT:     "spelling": "E"
// ENUM-NEXT:   },
// ENUM-NEXT:   {
// ENUM-NEXT:     "kind": "text",
// ENUM-NEXT:     "spelling": "<"
// ENUM-NEXT:   },
// ENUM-NEXT:   {
// ENUM-NEXT:     "kind": "genericParameter",
// ENUM-NEXT:     "spelling": "T"
// ENUM-NEXT:   },
// ENUM-NEXT:   {
// ENUM-NEXT:     "kind": "text",
// ENUM-NEXT:     "spelling": "> "
// ENUM-NEXT:   },
// ENUM-NEXT:   {
// ENUM-NEXT:     "kind": "keyword",
// ENUM-NEXT:     "spelling": "where"
// ENUM-NEXT:   },
// ENUM-NEXT:   {
// ENUM-NEXT:     "kind": "text",
// ENUM-NEXT:     "spelling": " T"
// ENUM-NEXT:   },
// ENUM-NEXT:   {
// ENUM-NEXT:     "kind": "text",
// ENUM-NEXT:     "spelling": " : "
// ENUM-NEXT:   },
// ENUM-NEXT:   {
// ENUM-NEXT:     "kind": "typeIdentifier",
// ENUM-NEXT:     "spelling": "Sequence",
// ENUM-NEXT:     "preciseIdentifier": "s:ST"
// ENUM-NEXT:   }
// ENUM-NEXT: ],

public typealias TA<T> = S<T> where T: Sequence

// TYPEALIAS-LABEL: "precise": "s:12NominalTypes2TAa",
// TYPEALIAS: "declarationFragments": [
// TYPEALIAS-NEXT:   {
// TYPEALIAS-NEXT:     "kind": "keyword",
// TYPEALIAS-NEXT:     "spelling": "typealias"
// TYPEALIAS-NEXT:   },
// TYPEALIAS-NEXT:   {
// TYPEALIAS-NEXT:     "kind": "text",
// TYPEALIAS-NEXT:     "spelling": " "
// TYPEALIAS-NEXT:   },
// TYPEALIAS-NEXT:   {
// TYPEALIAS-NEXT:     "kind": "identifier",
// TYPEALIAS-NEXT:     "spelling": "TA"
// TYPEALIAS-NEXT:   },
// TYPEALIAS-NEXT:   {
// TYPEALIAS-NEXT:     "kind": "text",
// TYPEALIAS-NEXT:     "spelling": "<"
// TYPEALIAS-NEXT:   },
// TYPEALIAS-NEXT:   {
// TYPEALIAS-NEXT:     "kind": "genericParameter",
// TYPEALIAS-NEXT:     "spelling": "T"
// TYPEALIAS-NEXT:   },
// TYPEALIAS-NEXT:   {
// TYPEALIAS-NEXT:     "kind": "text",
// TYPEALIAS-NEXT:     "spelling": "> = "
// TYPEALIAS-NEXT:   },
// TYPEALIAS-NEXT:   {
// TYPEALIAS-NEXT:     "kind": "typeIdentifier",
// TYPEALIAS-NEXT:     "spelling": "S",
// TYPEALIAS-NEXT:     "preciseIdentifier": "s:12NominalTypes1SV"
// TYPEALIAS-NEXT:   },
// TYPEALIAS-NEXT:   {
// TYPEALIAS-NEXT:     "kind": "text",
// TYPEALIAS-NEXT:     "spelling": "<T"
// TYPEALIAS-NEXT:   },
// TYPEALIAS-NEXT:   {
// TYPEALIAS-NEXT:     "kind": "text",
// TYPEALIAS-NEXT:     "spelling": "> "
// TYPEALIAS-NEXT:   },
// TYPEALIAS-NEXT:   {
// TYPEALIAS-NEXT:     "kind": "keyword",
// TYPEALIAS-NEXT:     "spelling": "where"
// TYPEALIAS-NEXT:   },
// TYPEALIAS-NEXT:   {
// TYPEALIAS-NEXT:     "kind": "text",
// TYPEALIAS-NEXT:     "spelling": " T"
// TYPEALIAS-NEXT:   },
// TYPEALIAS-NEXT:   {
// TYPEALIAS-NEXT:     "kind": "text",
// TYPEALIAS-NEXT:     "spelling": " : "
// TYPEALIAS-NEXT:   },
// TYPEALIAS-NEXT:   {
// TYPEALIAS-NEXT:     "kind": "typeIdentifier",
// TYPEALIAS-NEXT:     "spelling": "Sequence",
// TYPEALIAS-NEXT:     "preciseIdentifier": "s:ST"
// TYPEALIAS-NEXT:   }
// TYPEALIAS-NEXT: ],
