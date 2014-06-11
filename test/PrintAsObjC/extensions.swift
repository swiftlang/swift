// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -emit-module -o %t %s
// RUN: %swift %clang-importer-sdk -module-cache-path %t/clang-module-cache -parse-as-library %t/extensions.swiftmodule -parse -emit-objc-header-path %t/extensions.h
// RUN: FileCheck %s < %t/extensions.h
// RUN: FileCheck --check-prefix=NEGATIVE %s < %t/extensions.h
// RUN: %check-in-clang %t/extensions.h

import Foundation
import AppKit

// CHECK-NOT: AppKit

// CHECK: @interface A1{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class A1 {}

// CHECK: @interface A1 ()
// CHECK-NEXT: @end
extension A1 {}

// CHECK: @interface A2{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
// CHECK: @interface A2 ()
// CHECK-NEXT: @end
extension A2 {}
@objc class A2 {}

// NEGATIVE-NOT: NotObjC
class NotObjC {}
extension NotObjC {}

// CHECK: @interface NSObject ()
// CHECK-NEXT: @end
// NEGATIVE-NOT: @interface NSObject{{$}}
// NEGATIVE-NOT: @class NSObject
extension NSObject {}

// NEGATIVE-NOT: @class NSString;
// CHECK: @class NSColor;
// CHECK: @interface NSString ()
// CHECK-NEXT: - (void)test;
// CHECK-NEXT: + (void)test2;
// CHECK-NEXT: + (NSString *)fromColor:(NSColor *)color;
// CHECK-NEXT: @end
extension NSString {
  func test() {}
  class func test2() {}

  class func fromColor(color: NSColor) -> NSString? { return nil; }
}
