	
// REQUIRES: objc_interop
// REQUIRES: foundation

// RUN: %target-swift-frontend -enable-objc-interop -typecheck -verify  -import-objc-header %S/Inputs/availability_non_runtime_protocol_objc_protocol.h %s

import Foundation

@objc class Wrapper : NSObject {
  @objc public func runMutations(_ mutations: [ObjCNonRuntimeProtocolUsedinSwift]) { // expected-error {{Objective-C protocol 'ObjCNonRuntimeProtocolUsedinSwift' has the non_runtime_protocol attribute and its metadata is not available in Swift}}
    for _ in mutations {
    }
  }
}