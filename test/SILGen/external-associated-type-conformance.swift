// RUN: %target-swift-frontend -emit-silgen -import-objc-header %S/Inputs/external-associated-type-conformance.h %s -enable-sil-ownership | %FileCheck %s
// REQUIRES: objc_interop

extension BadError: LocalizedError {}

// This conformance...
// CHECK-LABEL: sil_witness_table shared [serialized] BadError.Code: _ErrorCodeProtocol module __ObjC 

// ...is used by the _BridgedStoredNSError conformance
// CHECK-LABEL: sil_witness_table shared [serialized] BadError: _BridgedStoredNSError module __ObjC {
// CHECK:           associated_type_protocol (Code: _ErrorCodeProtocol): BadError.Code: _ErrorCodeProtocol module __ObjC
// CHECK:       }

// ...so we have to emit both.
