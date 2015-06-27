// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/extensions.swiftmodule -parse -emit-objc-header-path %t/extensions.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: FileCheck %s < %t/extensions.h
// RUN: FileCheck --check-prefix=NEGATIVE %s < %t/extensions.h
// RUN: %check-in-clang %t/extensions.h

// REQUIRES: objc_interop

import Foundation
import AppKit

// CHECK-NOT: AppKit

// CHECK-LABEL: @interface A1{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class A1 {}

// CHECK-LABEL: @interface A1 (SWIFT_EXTENSION(extensions))
// CHECK-NEXT: @end
extension A1 {}

// CHECK-LABEL: @interface A2{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
// CHECK-LABEL: @interface A2 (SWIFT_EXTENSION(extensions))
// CHECK-NEXT: @end
extension A2 {}
@objc class A2 {}

// CHECK-LABEL: @interface A3{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class A3 {}

// CHECK-LABEL: @interface A3 (SWIFT_EXTENSION(extensions))
// CHECK-DAG: @interface A3 (SWIFT_EXTENSION(extensions))
// CHECK-DAG: @property (nonatomic, readonly) NSInteger more;
// CHECK-DAG: @property (nonatomic, readonly) NSInteger some;
// CHECK-DAG: @end
// CHECK: @end
extension A3 {
  var some: Int { return 1 }
}
extension A3 {
  var more: Int { return 10 }
}

// CHECK-LABEL: @interface A4{{$}}
// CHECK-NEXT: init
// CHECK-NEXT: @end
@objc class A4 {}

// CHECK-LABEL: @interface A4 (SWIFT_EXTENSION(extensions))
// CHECK-NEXT: @end
extension A4 {
  // CHECK-LABEL: @interface Inner
  // CHECK-NEXT: init
  // CHECK-NEXT: @end
  @objc class Inner {}
}

// NEGATIVE-NOT: CGColor
extension CGColor {
  func anyOldMethod() {}
}

// NEGATIVE-NOT: NotObjC
class NotObjC {}
extension NotObjC {}

// CHECK-LABEL: @interface NSObject (SWIFT_EXTENSION(extensions))
// CHECK-NEXT: @end
// NEGATIVE-NOT: @interface NSObject{{$}}
// NEGATIVE-NOT: @class NSObject
extension NSObject {}

// NEGATIVE-NOT: @class NSString;
// CHECK: @class NSColor;
// CHECK-LABEL: @interface NSString (SWIFT_EXTENSION(extensions))
// CHECK-NEXT: - (void)test;
// CHECK-NEXT: + (void)test2;
// CHECK-NEXT: + (NSString * __nullable)fromColor:(NSColor * __nonnull)color;
// CHECK-NEXT: @end
extension NSString {
  func test() {}
  class func test2() {}

  class func fromColor(color: NSColor) -> NSString? { return nil; }
}

