// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/witness_tables_serialized.swift -o %t -enable-library-evolution -package-name Package
// RUN: %target-swift-emit-silgen -I %t %s | %FileCheck %s

import witness_tables_serialized

public protocol AnotherPublicProtocol {}

@usableFromInline
internal protocol AnotherInternalProtocol {}

extension PublicResilientStruct : AnotherPublicProtocol, AnotherInternalProtocol {}

// CHECK: sil_witness_table [serialized] PublicResilientStruct: AnotherPublicProtocol
// CHECK: sil_witness_table [serialized] PublicResilientStruct: AnotherInternalProtocol
