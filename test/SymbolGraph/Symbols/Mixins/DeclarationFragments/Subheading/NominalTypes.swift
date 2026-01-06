// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name NominalTypes -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name NominalTypes -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/NominalTypes.symbols.json --check-prefix=STRUCT
// RUN: %FileCheck %s --input-file %t/NominalTypes.symbols.json --check-prefix=ENUM
// RUN: %FileCheck %s --input-file %t/NominalTypes.symbols.json --check-prefix=PROTOCOL
// RUN: %FileCheck %s --input-file %t/NominalTypes.symbols.json --check-prefix=CLASS
// RUN: %FileCheck %s --input-file %t/NominalTypes.symbols.json --check-prefix=TYPEALIAS

// STRUCT-LABEL: "precise": "s:12NominalTypes6StructV"
// STRUCT: subHeading
// STRUCT-NEXT: {
// STRUCT-NEXT:   "kind": "keyword",
// STRUCT-NEXT:   "spelling": "struct"
// STRUCT-NEXT: }
// STRUCT-NEXT: {
// STRUCT-NEXT:   "kind": "text",
// STRUCT-NEXT:   "spelling": " "
// STRUCT-NEXT: }
// STRUCT-NEXT: {
// STRUCT-NEXT:   "kind": "identifier",
// STRUCT-NEXT:   "spelling": "Struct"
// STRUCT-NEXT: }
public struct Struct<T> where T: Sequence {}

// ENUM-LABEL: "precise": "s:12NominalTypes4EnumO"
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
// ENUM-NEXT:   "kind": "identifier",
// ENUM-NEXT:   "spelling": "Enum"
// ENUM-NEXT: }
public enum Enum<T> where T: Sequence {}

// PROTOCOL-LABEL: "precise": "s:12NominalTypes8ProtocolP"
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
// PROTOCOL-NEXT:   "kind": "identifier",
// PROTOCOL-NEXT:   "spelling": "Protocol"
// PROTOCOL-NEXT: }
public protocol Protocol where T: Sequence {
  associatedtype T
}

// CLASS-LABEL: "precise": "s:12NominalTypes5ClassC"
// CLASS: subHeading
// CLASS-NEXT: {
// CLASS-NEXT:   "kind": "keyword",
// CLASS-NEXT:   "spelling": "class"
// CLASS-NEXT: },
// CLASS-NEXT: {
// CLASS-NEXT:   "kind": "text",
// CLASS-NEXT:   "spelling": " "
// CLASS-NEXT: },
// CLASS-NEXT: {
// CLASS-NEXT:   "kind": "identifier",
// CLASS-NEXT:   "spelling": "Class"
// CLASS-NEXT: }
public class Class<T> where T: Sequence {}

// TYPEALIAS-LABEL: "precise": "s:12NominalTypes9TypeAliasa"
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
// TYPEALIAS-NEXT:   "kind": "identifier",
// TYPEALIAS-NEXT:   "spelling": "TypeAlias"
// TYPEALIAS-NEXT: }
public typealias TypeAlias<T> = Struct<T> where T: Collection

