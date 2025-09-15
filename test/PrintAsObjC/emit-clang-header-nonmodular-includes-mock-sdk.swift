// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -F %S/Inputs/ -typecheck -verify -emit-objc-header-path %t/textual-imports.h -emit-clang-header-nonmodular-includes %s 
// RUN: %FileCheck %s < %t/textual-imports.h
// RUN: %check-in-clang -fno-modules -Qunused-arguments %t/textual-imports.h -F %S/Inputs
// RUN: %check-in-clang-c %t/textual-imports.h -F %S/Inputs

import Foundation
import Mixed

public class HelloWorld: NSObject {
  @objc public func sayHello() {
    print("Hello, World!")
  }

  @objc public func getPoint() -> CGPoint {
    return CGPoint(x: 1, y: 1)
  }

  @objc public func getIntAlias() -> CIntAlias {
    let result: CInt = 0
    return result
  }
}

// CHECK:      #if __has_feature(objc_modules)
// CHECK-NEXT: #if __has_warning("-Watimport-in-framework-header")
// CHECK-NEXT: #pragma clang diagnostic ignored "-Watimport-in-framework-header"
// CHECK-NEXT: #endif
// CHECK-NEXT: @import CoreGraphics;
// CHECK-NEXT: @import Mixed;
// CHECK-NEXT: @import ObjectiveC;
// CHECK-NEXT: #elif defined(__OBJC__)
// CHECK-NEXT: #import <CoreGraphics.h>
// CHECK-NEXT: #import <Mixed/Mixed.h>
// CHECK-NEXT: #import <objc/objc.h>
// CHECK-NEXT: #import <objc/NSObject.h>
// CHECK-NEXT: #else
// CHECK-NEXT: #include <CoreGraphics.h>
// CHECK-NEXT: #include <Mixed/Mixed.h>
// CHECK-NEXT: #include <objc/objc.h>
// CHECK-NEXT: #include <objc/NSObject.h>
// CHECK-NEXT: #endif
