/// Include module for use from both C and Objective-C @cdecl variants.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

/// Generate the compatibility header cdecl.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/Lib.swift \
// RUN:   -emit-module -o %t -verify -F %t \
// RUN:   -emit-clang-header-path %t/cdecl.h \
// RUN:   -enable-experimental-feature CDecl

/// Check compatibility header directly
// RUN: %FileCheck %s --input-file %t/cdecl.h
// RUN: %check-in-clang %t/cdecl.h -F %t
// RUN: %check-in-clang-c %t/cdecl.h -F %t
// RUN: %check-in-clang-cxx %t/cdecl.h -F %t

// REQUIRES: swift_feature_CDecl
// REQUIRES: objc_interop

//--- CFramework.framework/Modules/module.modulemap

framework module CFramework {
    umbrella header "CFramework.h"
}

//--- CFramework.framework/Headers/CFramework.h

typedef int IntFromCFramework;

//--- Lib.swift

import CFramework
import CoreGraphics
import Foundation

// CHECK-NOT: Foundation;

/// Imports for C variant to @_cdecl

// CHECK: #if __has_feature(objc_modules)
// CHECK: @import CFramework;
// CHECK-NEXT: @import CoreGraphics;
// CHECK-NEXT: #elif defined(__OBJC__)
// CHECK-NEXT: #import <CFramework{{[/\\]}}CFramework.h>
// CHECK-NEXT: #import <CoreGraphics.h>
// CHECK-NEXT: #else
// CHECK-NEXT: #include <CFramework{{[/\\]}}CFramework.h>
// CHECK-NEXT: #include <CoreGraphics.h>
// CHECK-NEXT: #endif

// CHECK: #if defined(__cplusplus)
// CHECK: extern "C" {
// CHECK: #endif

@cdecl(get_int_alias)
public func getIntAlias() -> IntFromCFramework { 42 }
// CHECK: SWIFT_EXTERN IntFromCFramework get_int_alias(void) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;

@cdecl(imports_cgpoint)
public func importsCGPoint(pt: CGPoint) {  }
// CHECK: SWIFT_EXTERN void imports_cgpoint(CGPoint pt) SWIFT_NOEXCEPT;

// CHECK: #if defined(__cplusplus)
// CHECK: } // extern "C"
// CHECK: #endif

/// Imports for Objective-C variant to @_cdecl

@_cdecl("imports_cgpoint_objc")
public func importsCGPointObjC(pt: CGPoint) {  }
// CHECK: #if defined(__OBJC__)
// CHECK: #if __has_feature(objc_modules)
// CHECK-NEXT: #if __has_warning("-Watimport-in-framework-header")
// CHECK-NEXT: #pragma clang diagnostic ignored "-Watimport-in-framework-header"
// CHECK-NEXT: #endif
// CHECK-NEXT: @import CoreGraphics;
// CHECK-NEXT: #endif

// CHECK: #if defined(__OBJC__)
// CHECK: SWIFT_EXTERN void imports_cgpoint_objc(CGPoint pt) SWIFT_NOEXCEPT;
// CHECK: #endif
