// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -parse-as-library %s -parse -emit-objc-header-path %t/accessibility.h -disable-objc-attr-requires-foundation-module
// RUN: FileCheck -check-prefix=CHECK -check-prefix=CHECK-PUBLIC %s < %t/accessibility.h
// RUN: %check-in-clang %t/accessibility.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -parse -emit-objc-header-path %t/accessibility-internal.h -disable-objc-attr-requires-foundation-module
// RUN: FileCheck -check-prefix=CHECK -check-prefix=CHECK-INTERNAL %s < %t/accessibility-internal.h
// RUN: %check-in-clang %t/accessibility-internal.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %s -parse -import-objc-header %S/../Inputs/empty.h -emit-objc-header-path %t/accessibility-imported-header.h -disable-objc-attr-requires-foundation-module
// RUN: FileCheck -check-prefix=CHECK -check-prefix=CHECK-INTERNAL %s < %t/accessibility-imported-header.h
// RUN: %check-in-clang %t/accessibility-imported-header.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %s -parse -DMAIN -emit-objc-header-path %t/accessibility-main.h -disable-objc-attr-requires-foundation-module
// RUN: FileCheck -check-prefix=CHECK -check-prefix=CHECK-INTERNAL %s < %t/accessibility-main.h
// RUN: %check-in-clang %t/accessibility-main.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %s -parse -application-extension -emit-objc-header-path %t/accessibility-appext.h -disable-objc-attr-requires-foundation-module
// RUN: FileCheck -check-prefix=CHECK -check-prefix=CHECK-INTERNAL %s < %t/accessibility-appext.h
// RUN: %check-in-clang %t/accessibility-appext.h

// REQUIRES: objc_interop

// CHECK-LABEL: @interface A_Public{{$}}
// CHECK-INTERNAL-NEXT: init
// CHECK-NEXT: @end
@objc public class A_Public {}

// CHECK-PUBLIC-NOT: B_Internal
// CHECK-INTERNAL-LABEL: @interface B_Internal{{$}}
// CHECK-INTERNAL-NEXT: init
// CHECK-INTERNAL-NEXT: @end
@objc internal class B_Internal {}

// CHECK-NOT: C_Private
@objc private class C_Private {}


#if MAIN
#if os(OSX)
import AppKit

@NSApplicationMain
@objc class AppDelegate : NSApplicationDelegate {}

#elseif os(iOS)
import UIKit

@UIApplicationMain
@objc class AppDelegate : NSObject, UIApplicationDelegate {}

#else
// Uh oh, this test depends on having an app delegate.
#endif
#endif
