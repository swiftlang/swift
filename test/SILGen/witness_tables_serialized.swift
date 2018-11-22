// This file is also used by witness_tables_serialized_import.swift.

// RUN: %target-swift-emit-silgen -enable-sil-ownership %s -emit-sorted-sil | %FileCheck -check-prefix CHECK -check-prefix CHECK-NONRESILIENT %s
// RUN: %target-swift-emit-silgen -enable-sil-ownership -enable-resilience %s -emit-sorted-sil | %FileCheck -check-prefix CHECK -check-prefix CHECK-RESILIENT %s

public protocol PublicProtocol {}

@usableFromInline
internal protocol InternalProtocol {}

@_fixed_layout
public struct PublicStruct : PublicProtocol, InternalProtocol {}

public struct PublicResilientStruct : PublicProtocol, InternalProtocol {}

@usableFromInline
internal struct InternalStruct : PublicProtocol, InternalProtocol {}

// CHECK: sil_witness_table [serialized] PublicStruct: PublicProtocol
// CHECK: sil_witness_table [serialized] PublicStruct: InternalProtocol

// CHECK-NONRESILIENT: sil_witness_table [serialized] InternalStruct: InternalProtocol
// CHECK-NONRESILIENT: sil_witness_table [serialized] InternalStruct: PublicProtocol
//
// CHECK-RESILIENT: sil_witness_table InternalStruct: InternalProtocol
// CHECK-RESILIENT: sil_witness_table InternalStruct: PublicProtocol

// CHECK-NONRESILIENT: sil_witness_table [serialized] PublicResilientStruct: PublicProtocol
// CHECK-NONRESILIENT: sil_witness_table [serialized] PublicResilientStruct: InternalProtocol
//
// CHECK-RESILIENT: sil_witness_table PublicResilientStruct: PublicProtocol
// CHECK-RESILIENT: sil_witness_table PublicResilientStruct: InternalProtocol
