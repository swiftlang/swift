// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: swift2objc %clang-importer-sdk -module-cache-path=%t/clang-module-cache %s -o %t/extensions.h
// RUN: FileCheck %s < %t/extensions.h
// RUN: FileCheck --check-prefix=NEGATIVE %s < %t/extensions.h

import Foundation

// CHECK: @interface A
// CHECK-NEXT: @end
@objc class A {}

// CHECK: @interface A ()
// CHECK-NEXT: @end
extension A {}

// CHECK: @interface A2{{$}}
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
