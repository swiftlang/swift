// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %s | %FileCheck %s

public protocol PublicProtocol {}

@usableFromInline
internal protocol InternalProtocol {}

@_fixed_layout
public struct PublicStruct : PublicProtocol, InternalProtocol {}

@usableFromInline
internal struct InternalStruct : PublicProtocol, InternalProtocol {}

// CHECK-LABEL: sil_witness_table [serialized] PublicStruct: PublicProtocol
// CHECK-LABEL: sil_witness_table [serialized] PublicStruct: InternalProtocol

// CHECK-LABEL: sil_witness_table [serialized] InternalStruct: PublicProtocol
// CHECK-LABEL: sil_witness_table [serialized] InternalStruct: InternalProtocol
