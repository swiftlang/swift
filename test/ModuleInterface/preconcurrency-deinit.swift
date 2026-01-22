// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -module-name SwiftLibrary %t/SwiftLibrary.swift -emit-module -emit-module-interface-path %t/SwiftLibrary.swiftinterface -swift-version 6 -I %t/ObjCLibrary -experimental-allow-module-with-compiler-errors
// Added experimental-allow-module-with-compiler-errors flag to avoid crashes on the assert in serialization
// RUN: %FileCheck %s < %t/SwiftLibrary.swiftinterface

//--- SwiftLibrary.swift
import ObjCLibrary

// CHECK: {{^ @objc isolated deinit$}}
public class SwiftSubclass: ObjCBaseClass {
    isolated deinit {}
}

//--- ObjCLibrary/module.modulemap
module ObjCLibrary {
  header "ObjCBaseClass.h"
  export *
}

//--- ObjCLibrary/ObjCBaseClass.h

#import <Foundation/Foundation.h>

__attribute__((__swift_attr__("@MainActor")))
@interface ObjCBaseClass : NSObject
@end
