// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership -sil-serialize-witness-tables %s | %FileCheck %s

public protocol PublicProtocol {}

internal protocol InternalProtocol {}

public struct PublicStruct : PublicProtocol, InternalProtocol {}

internal struct InternalStruct : PublicProtocol, InternalProtocol {}

// CHECK-LABEL: sil_witness_table [serialized] PublicStruct: PublicProtocol
// CHECK-LABEL: sil_witness_table hidden PublicStruct: InternalProtocol

// CHECK-LABEL: sil_witness_table hidden InternalStruct: PublicProtocol
// CHECK-LABEL: sil_witness_table hidden InternalStruct: InternalProtocol
