	
// REQUIRES: objc_interop
// REQUIRES: foundation

// RUN: %target-swift-frontend -enable-objc-interop -typecheck -verify  -import-objc-header %S/Inputs/availability_non_runtime_protocol_objc_protocol.h %s -emit-objc-header -emit-objc-header-path %t/SwiftCallObjC-Swift.h 

import Foundation

class AppDelegate {
    @inline(never) func FOOBARwrapper() {
        // CustomObject is defined in ObjC.
        let instanceOfCustomObject = CustomObject()
        instanceOfCustomObject.someProperty = "Hello World"
        print(instanceOfCustomObject.someProperty)
        instanceOfCustomObject.someMethod()
    }
}

@objc class Wrapper : NSObject {
  @objc public func runMutations(_ mutations: [ComposerMutationBridging]) { // expected-error {{this Objective-C protocol 'ComposerMutationBridging' has the non_runtime_protocol attribute and its metadata is not available in Swift.}}
    for _ in mutations {
    }
  }
}

var tmp = AppDelegate()
tmp.FOOBARwrapper()