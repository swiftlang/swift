// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-objc-header-path %t/textual-imports.h -emit-clang-header-nonmodular-includes %s
// RUN: %FileCheck %s < %t/textual-imports.h --check-prefixes=CHECK,CHECK-%target-sdk-name
// RUN: %clang -fsyntax-only -x objective-c-header -fno-modules -isysroot %sdk %t/textual-imports.h

import CoreGraphics
import Darwin.C.time
import Foundation
import zlib

public class HelloWorld: NSObject {
  @objc public func takeGzFile(file: gzFile) {
  }

  @objc public func takeTime(time: tm) {
  }

  @objc public func getPoint() -> CGPoint {
    return CGPoint(x: 1, y: 1)
  }
}

// Verify that these modules, and only these modules are @imported

// CHECK:      #if __has_feature(objc_modules)
// CHECK-NEXT: #if __has_warning("-Watimport-in-framework-header")
// CHECK-NEXT: #pragma clang diagnostic ignored "-Watimport-in-framework-header"
// CHECK-NEXT: #endif
// CHECK-NEXT: @import CoreGraphics;
// CHECK-NEXT: @import Darwin;
// CHECK-NEXT: @import ObjectiveC;
// CHECK-NEXT: @import zlib;
// CHECK-NEXT: #else

// Verify that exactly the set of headers included in the
// the imported modules on the current platform are included
// and that they are included in alphabetic order

// CHECK-macos:      #else
// CHECK-macos-NEXT: #import <CoreGraphics/CoreGraphics.h>
// CHECK-macos-NEXT: #import <objc/List.h>
// CHECK-macos-NEXT: #import <objc/NSObjCRuntime.h>
// CHECK-macos-NEXT: #import <objc/NSObject.h>
// CHECK-macos-NEXT: #import <objc/Object.h>
// CHECK-macos-NEXT: #import <objc/Protocol.h>
// CHECK-macos-NEXT: #import <objc/hashtable.h>
// CHECK-macos-NEXT: #import <objc/hashtable2.h>
// CHECK-macos-NEXT: #import <objc/message.h>
// CHECK-macos-NEXT: #import <objc/objc-api.h>
// CHECK-macos-NEXT: #import <objc/objc-auto.h>
// CHECK-macos-NEXT: #import <objc/objc-class.h>
// CHECK-macos-NEXT: #import <objc/objc-exception.h>
// CHECK-macos-NEXT: #import <objc/objc-load.h>
// CHECK-macos-NEXT: #import <objc/objc-runtime.h>
// CHECK-macos-NEXT: #import <objc/objc-sync.h>
// CHECK-macos-NEXT: #import <objc/objc.h>
// CHECK-macos-NEXT: #import <objc/runtime.h>
// CHECK-macos-NEXT: #import <zconf.h>
// CHECK-macos-NEXT: #import <zlib.h>
// CHECK-macos-NEXT: #endif

// CHECK-iphoneos:      #else
// CHECK-iphoneos-NEXT: #import <objc/NSObjCRuntime.h>
// CHECK-iphoneos-NEXT: #import <objc/NSObject.h>
// CHECK-iphoneos-NEXT: #import <objc/message.h>
// CHECK-iphoneos-NEXT: #import <objc/objc-api.h>
// CHECK-iphoneos-NEXT: #import <objc/objc-auto.h>
// CHECK-iphoneos-NEXT: #import <objc/objc-exception.h>
// CHECK-iphoneos-NEXT: #import <objc/objc-sync.h>
// CHECK-iphoneos-NEXT: #import <objc/objc.h>
// CHECK-iphoneos-NEXT: #import <objc/runtime.h>
// CHECK-iphoneos-NEXT: #import <zconf.h>
// CHECK-iphoneos-NEXT: #import <zlib.h>
// CHECK-iphoneos-NEXT: #endif

// CHECK-iphoneossimulator:      #else
// CHECK-iphoneossimulator-NEXT: #import <CoreGraphics/CoreGraphics.h>
// CHECK-iphoneossimulator-NEXT: #import <objc/NSObjCRuntime.h>
// CHECK-iphoneossimulator-NEXT: #import <objc/NSObject.h>
// CHECK-iphoneossimulator-NEXT: #import <objc/message.h>
// CHECK-iphoneossimulator-NEXT: #import <objc/objc-api.h>
// CHECK-iphoneossimulator-NEXT: #import <objc/objc-auto.h>
// CHECK-iphoneossimulator-NEXT: #import <objc/objc-exception.h>
// CHECK-iphoneossimulator-NEXT: #import <objc/objc-sync.h>
// CHECK-iphoneossimulator-NEXT: #import <objc/objc.h>
// CHECK-iphoneossimulator-NEXT: #import <objc/runtime.h>
// CHECK-iphoneossimulator-NEXT: #import <zconf.h>
// CHECK-iphoneossimulator-NEXT: #import <zlib.h>
// CHECK-iphoneossimulator-NEXT: #endif
