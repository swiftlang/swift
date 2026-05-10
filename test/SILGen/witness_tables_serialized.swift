// This file is also used by witness_tables_serialized_import.swift.

// RUN: %target-swift-emit-silgen %s -package-name Package | %FileCheck -check-prefix CHECK -check-prefix CHECK-NONRESILIENT %s
// RUN: %target-swift-emit-silgen -enable-library-evolution %s -package-name Package | %FileCheck -check-prefix CHECK -check-prefix CHECK-RESILIENT %s

public protocol PublicProtocol {}

package protocol PackageProtocol {}

@usableFromInline internal protocol UsableFromInlineProtocol {}

internal protocol InternalProtocol {}

@frozen
public struct PublicFrozenStruct : PublicProtocol, UsableFromInlineProtocol, PackageProtocol, InternalProtocol {}

public struct PublicResilientStruct : PublicProtocol, UsableFromInlineProtocol, PackageProtocol, InternalProtocol {}

package struct PackageStruct : PublicProtocol, UsableFromInlineProtocol, PackageProtocol, InternalProtocol {}

@usableFromInline
internal struct UsableFromInlineStruct : PublicProtocol, UsableFromInlineProtocol, PackageProtocol, InternalProtocol {}

// CHECK-DAG: sil_witness_table [serialized] PublicFrozenStruct: PublicProtocol
// CHECK-DAG: sil_witness_table [serialized] PublicFrozenStruct: UsableFromInlineProtocol
// CHECK-DAG: sil_witness_table package PublicFrozenStruct: PackageProtocol
// CHECK-DAG: sil_witness_table hidden PublicFrozenStruct: InternalProtocol

// CHECK-RESILIENT-DAG: sil_witness_table UsableFromInlineStruct: UsableFromInlineProtocol
// CHECK-RESILIENT-DAG: sil_witness_table UsableFromInlineStruct: PublicProtocol
// CHECK-RESILIENT-DAG: sil_witness_table package UsableFromInlineStruct: PackageProtocol
// CHECK-RESILIENT-DAG: sil_witness_table hidden UsableFromInlineStruct: InternalProtocol

// CHECK-RESILIENT-DAG: sil_witness_table PublicResilientStruct: PublicProtocol
// CHECK-RESILIENT-DAG: sil_witness_table PublicResilientStruct: UsableFromInlineProtocol
// CHECK-RESILIENT-DAG: sil_witness_table package PublicResilientStruct: PackageProtocol
// CHECK-RESILIENT-DAG: sil_witness_table hidden PublicResilientStruct: InternalProtocol

// CHECK-RESILIENT-DAG: sil_witness_table package PackageStruct: PublicProtocol
// CHECK-RESILIENT-DAG: sil_witness_table package PackageStruct: UsableFromInlineProtocol
// CHECK-RESILIENT-DAG: sil_witness_table package PackageStruct: PackageProtocol
// CHECK-RESILIENT-DAG: sil_witness_table hidden PackageStruct: InternalProtocol

// CHECK-NONRESILIENT-DAG: sil_witness_table [serialized] UsableFromInlineStruct: UsableFromInlineProtocol
// CHECK-NONRESILIENT-DAG: sil_witness_table [serialized] UsableFromInlineStruct: PublicProtocol
// CHECK-NONRESILIENT-DAG: sil_witness_table package UsableFromInlineStruct: PackageProtocol
// CHECK-NONRESILIENT-DAG: sil_witness_table hidden UsableFromInlineStruct: InternalProtocol

// CHECK-NONRESILIENT-DAG: sil_witness_table [serialized] PublicResilientStruct: PublicProtocol
// CHECK-NONRESILIENT-DAG: sil_witness_table [serialized] PublicResilientStruct: UsableFromInlineProtocol
// CHECK-NONRESILIENT-DAG: sil_witness_table package PublicResilientStruct: PackageProtocol
// CHECK-NONRESILIENT-DAG: sil_witness_table hidden PublicResilientStruct: InternalProtocol

// CHECK-NONRESILIENT-DAG: sil_witness_table package PackageStruct: PublicProtocol
// CHECK-NONRESILIENT-DAG: sil_witness_table package PackageStruct: UsableFromInlineProtocol
// CHECK-NONRESILIENT-DAG: sil_witness_table package PackageStruct: PackageProtocol
// CHECK-NONRESILIENT-DAG: sil_witness_table hidden PackageStruct: InternalProtocol
