 // REQUIRES: objc_interop
// REQUIRES: foundation

// RUN: %target-swift-frontend -enable-objc-interop -typecheck -verify  -import-objc-header %S/Inputs/availability_non_runtime_protocol_objc_protocol.h %s

 
 
    let _ : ObjCNonRuntimeProtocolUsedinSwift