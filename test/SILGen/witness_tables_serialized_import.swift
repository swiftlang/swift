// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-sil-ownership -emit-module %S/witness_tables_serialized.swift -o %t -enable-resilience
// RUN: %target-swift-emit-silgen -enable-sil-ownership -I %t %s | %FileCheck %s

import witness_tables_serialized

public protocol AnotherPublicProtocol {}

@usableFromInline
internal protocol AnotherInternalProtocol {}

extension PublicResilientStruct : AnotherPublicProtocol, AnotherInternalProtocol {}

// CHECK: sil_witness_table [serialized] PublicResilientStruct: AnotherPublicProtocol
// CHECK: sil_witness_table [serialized] PublicResilientStruct: AnotherInternalProtocol
