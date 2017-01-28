// RUN: %target-swift-frontend -emit-silgen -import-objc-header %S/Inputs/external-associated-type-conformance.h %s | %FileCheck %s
// REQUIRES: objc_interop

extension BadError: LocalizedError {}

// -- The _BridgedStoredNSError conformance...
// CHECK-LABEL: sil_witness_table shared [fragile] BadError: _BridgedStoredNSError module __ObjC {
// -- uses the `Code` type's _ErrorCodeProtocol conformance...
// CHECK:           associated_type_protocol (Code: _ErrorCodeProtocol): BadError.Code: _ErrorCodeProtocol module __ObjC
// CHECK:       }
// -- so we have to emit that too
// CHECK-LABEL: sil_witness_table shared [fragile] BadError.Code: _ErrorCodeProtocol module __ObjC 
