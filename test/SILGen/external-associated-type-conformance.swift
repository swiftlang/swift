// RUN: %target-swift-emit-silgen -import-objc-header %S/Inputs/external-associated-type-conformance.h %s | %FileCheck %s
// REQUIRES: objc_interop

extension BadError: LocalizedError {}

// -- The _BridgedStoredNSError conformance...
// CHECK-LABEL: sil_witness_table shared [serialized] BadError: _BridgedStoredNSError module __ObjC {
// -- uses the `Code` type's _ErrorCodeProtocol conformance...
// CHECK:           associated_conformance (Code: _ErrorCodeProtocol): BadError.Code: _ErrorCodeProtocol module __ObjC
// CHECK:       }
// -- so we have to emit that too
// CHECK-LABEL: sil_witness_table shared [serialized] BadError.Code: _ErrorCodeProtocol module __ObjC 
