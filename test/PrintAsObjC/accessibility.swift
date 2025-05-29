// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library %s -typecheck -verify -emit-objc-header-path %t/accessibility.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=CHECK-PUBLIC %s < %t/accessibility.h
// RUN: %check-in-clang %t/accessibility.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -verify -emit-objc-header-path %t/accessibility-internal.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=CHECK-INTERNAL %s < %t/accessibility-internal.h
// RUN: %check-in-clang %t/accessibility-internal.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %s -import-objc-header %S/../Inputs/empty.h -typecheck -verify -emit-objc-header-path %t/accessibility-imported-header.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=CHECK-INTERNAL %s < %t/accessibility-imported-header.h
// RUN: %check-in-clang %t/accessibility-imported-header.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %s -DMAIN -typecheck -verify -emit-objc-header-path %t/accessibility-main.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=CHECK-INTERNAL %s < %t/accessibility-main.h
// RUN: %check-in-clang %t/accessibility-main.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %s -application-extension -typecheck -verify -emit-objc-header-path %t/accessibility-appext.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=CHECK-INTERNAL %s < %t/accessibility-appext.h
// RUN: %check-in-clang %t/accessibility-appext.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %s -application-extension-library -typecheck -verify -emit-objc-header-path %t/accessibility-appextlib.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=CHECK-PUBLIC %s < %t/accessibility-appextlib.h
// RUN: %check-in-clang %t/accessibility-appextlib.h

// REQUIRES: objc_interop

// CHECK: #ifndef ACCESSIBILITY_SWIFT_H
// CHECK-NEXT: #define ACCESSIBILITY_SWIFT_H

// CHECK-LABEL: @interface A_Public{{$}}
// CHECK-INTERNAL-NEXT: init
// CHECK-NEXT: @end
@objc @objcMembers public class A_Public {}

// CHECK-PUBLIC-NOT: B_Internal
// CHECK-INTERNAL-LABEL: @interface B_Internal{{$}}
// CHECK-INTERNAL-NEXT: init
// CHECK-INTERNAL-NEXT: @end
@objc @objcMembers internal class B_Internal {}

// CHECK-NOT: C_Private
@objc @objcMembers private class C_Private {}


#if MAIN
#if canImport(AppKit)
import AppKit

@NSApplicationMain
@objc class AppDelegate : NSApplicationDelegate {}

#elseif canImport(UIKit)
import UIKit

@UIApplicationMain
@objc class AppDelegate : NSObject, UIApplicationDelegate {}

#else
// Uh oh, this test depends on having an app delegate.
#error("Unsupported platform")
#endif
#endif
