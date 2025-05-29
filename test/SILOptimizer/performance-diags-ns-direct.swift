// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -verify -I %t/Inputs %t/test.swift 

// REQUIRES: objc_interop

//--- Inputs/header.h

@interface Interface

- (void)test __attribute__((objc_direct)) __attribute__((swift_attr("@_noObjCBridging")));

@end


//--- Inputs/module.modulemap

module ObjCModule {
    header "header.h"
    export *
}

//--- test.swift

import Foundation
import ObjCModule

@_noObjCBridging
func test(i: Interface) {
  i.test()
}