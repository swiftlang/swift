// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name SubheadingDeclarationFragmentsTypes -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name SubheadingDeclarationFragmentsTypes -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/SubheadingDeclarationFragmentsTypes.symbols.json --check-prefix=STRUCT
// RUN: %FileCheck %s --input-file %t/SubheadingDeclarationFragmentsTypes.symbols.json --check-prefix=ENUM
// RUN: %FileCheck %s --input-file %t/SubheadingDeclarationFragmentsTypes.symbols.json --check-prefix=PROTOCOL
// RUN: %FileCheck %s --input-file %t/SubheadingDeclarationFragmentsTypes.symbols.json --check-prefix=CLASS
// RUN: %FileCheck %s --input-file %t/SubheadingDeclarationFragmentsTypes.symbols.json --check-prefix=TYPEALIAS

// STRUCT-LABEL: "precise": "s:35SubheadingDeclarationFragmentsTypes6StructV"
// STRUCT: subHeading
// STRUCT-NEXT {
// STRUCT-NEXT   "kind": "keyword",
// STRUCT-NEXT   "spelling": "struct"
// STRUCT-NEXT }
// STRUCT-NEXT {
// STRUCT-NEXT   "kind": "text",
// STRUCT-NEXT   "spelling": " "
// STRUCT-NEXT }
// STRUCT-NEXT {
// STRUCT-NEXT   "kind": "typeIdentifier",
// STRUCT-NEXT   "spelling": "Struct",
// STRUCT-NEXT   "preciseIdentifier": "s:35SubheadingDeclarationFragmentsTypes6StructV"
// STRUCT-NEXT }
public struct Struct<T> where T: Sequence {}

// ENUM-LABEL: "precise": "s:35SubheadingDeclarationFragmentsTypes4EnumO"
// ENUM: subHeading
// ENUM-NEXT: {
// ENUM-NEXT:   "kind": "keyword",
// ENUM-NEXT:   "spelling": "enum"
// ENUM-NEXT: }
// ENUM-NEXT: {
// ENUM-NEXT:   "kind": "text",
// ENUM-NEXT:   "spelling": " "
// ENUM-NEXT: }
// ENUM-NEXT: {
// ENUM-NEXT:   "kind": "typeIdentifier",
// ENUM-NEXT:   "spelling": "Enum",
// ENUM-NEXT:   "preciseIdentifier": "s:35SubheadingDeclarationFragmentsTypes4EnumO"
// ENUM-NEXT: }
public enum Enum<T> where T: Sequence {}

// PROTOCOL-LABEL: "precise": "s:35SubheadingDeclarationFragmentsTypes8ProtocolP"
// PROTOCOL: subHeading
// PROTOCOL-NEXT: {
// PROTOCOL-NEXT:   "kind": "keyword",
// PROTOCOL-NEXT:   "spelling": "protocol"
// PROTOCOL-NEXT: }
// PROTOCOL-NEXT: {
// PROTOCOL-NEXT:   "kind": "text",
// PROTOCOL-NEXT:   "spelling": " "
// PROTOCOL-NEXT: }
// PROTOCOL-NEXT: {
// PROTOCOL-NEXT:   "kind": "typeIdentifier",
// PROTOCOL-NEXT:   "spelling": "Protocol",
// PROTOCOL-NEXT:   "preciseIdentifier": "s:35SubheadingDeclarationFragmentsTypes8ProtocolP"
// PROTOCOL-NEXT: }
public protocol Protocol where T: Sequence {
  associatedtype T
}

// CLASS-LABEL: "precise": "s:35SubheadingDeclarationFragmentsTypes5ClassC"
// CLASS: subHeading
// CLASS-NEXT {
// CLASS-NEXT   "kind": "keyword",
// CLASS-NEXT   "spelling": "class"
// CLASS-NEXT },
// CLASS-NEXT {
// CLASS-NEXT   "kind": "text",
// CLASS-NEXT   "spelling": " "
// CLASS-NEXT },
// CLASS-NEXT {
// CLASS-NEXT   "kind": "typeIdentifier",
// CLASS-NEXT   "spelling": "Class",
// CLASS-NEXT   "preciseIdentifier": "s:35SubheadingDeclarationFragmentsTypes5ClassC"
// CLASS-NEXT }
public class Class<T> where T: Sequence {}

// TYPEALIAS-LABEL: "precise": "s:35SubheadingDeclarationFragmentsTypes9TypeAliasa"
// TYPEALIAS: subHeading
// TYPEALIAS-NEXT: {
// TYPEALIAS-NEXT:   "kind": "keyword",
// TYPEALIAS-NEXT:   "spelling": "typealias"
// TYPEALIAS-NEXT: },
// TYPEALIAS-NEXT: {
// TYPEALIAS-NEXT:   "kind": "text",
// TYPEALIAS-NEXT:   "spelling": " "
// TYPEALIAS-NEXT: },
// TYPEALIAS-NEXT: {
// TYPEALIAS-NEXT:   "kind": "typeIdentifier",
// TYPEALIAS-NEXT:   "spelling": "TypeAlias",
// TYPEALIAS-NEXT:   "preciseIdentifier": "s:35SubheadingDeclarationFragmentsTypes9TypeAliasa"
// TYPEALIAS-NEXT: }
public typealias TypeAlias<T> = Struct<T> where T: Collection

