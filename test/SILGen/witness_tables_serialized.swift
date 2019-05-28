// This file is also used by witness_tables_serialized_import.swift.

// RUN: %target-swift-emit-silgen %s | %FileCheck -check-prefix CHECK -check-prefix CHECK-NONRESILIENT %s
// RUN: %target-swift-emit-silgen -enable-library-evolution %s | %FileCheck -check-prefix CHECK -check-prefix CHECK-RESILIENT %s

public protocol PublicProtocol {}

@usableFromInline
internal protocol InternalProtocol {}

@_fixed_layout
public struct PublicStruct : PublicProtocol, InternalProtocol {}

public struct PublicResilientStruct : PublicProtocol, InternalProtocol {}

@usableFromInline
internal struct InternalStruct : PublicProtocol, InternalProtocol {}

// CHECK-DAG: sil_witness_table [serialized] PublicStruct: PublicProtocol
// CHECK-DAG: sil_witness_table [serialized] PublicStruct: InternalProtocol

// CHECK-RESILIENT-DAG: sil_witness_table InternalStruct: InternalProtocol
// CHECK-RESILIENT-DAG: sil_witness_table InternalStruct: PublicProtocol

// CHECK-RESILIENT-DAG: sil_witness_table PublicResilientStruct: PublicProtocol
// CHECK-RESILIENT-DAG: sil_witness_table PublicResilientStruct: InternalProtocol

// CHECK-NONRESILIENT-DAG: sil_witness_table [serialized] InternalStruct: InternalProtocol
// CHECK-NONRESILIENT-DAG: sil_witness_table [serialized] InternalStruct: PublicProtocol

// CHECK-NONRESILIENT-DAG: sil_witness_table [serialized] PublicResilientStruct: PublicProtocol
// CHECK-NONRESILIENT-DAG: sil_witness_table [serialized] PublicResilientStruct: InternalProtocol
