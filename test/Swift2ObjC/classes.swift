// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: swift2objc %clang-importer-sdk -module-cache-path=%t/clang-module-cache %s -o %t/classes.h
// RUN: FileCheck %s < %t/classes.h
// RUN: FileCheck --check-prefix=NEGATIVE %s < %t/classes.h

import Foundation

// CHECK: @interface A
// CHECK-NEXT: @end
@objc class A {}

// CHECK: @interface B
// CHECK-NEXT: @end
@objc class B : A {}

// FIXME: We need to re-order these.
// CHECK: @interface B2
// CHECK-NEXT: @end
// CHECK: @interface A2
// CHECK-NEXT: @end
@objc class B2 : A2 {}
@objc class A2 {}

// NEGATIVE-NOT: NotObjC
class NotObjC {}

// CHECK: @interface MyObject
// CHECK-NEXT: @end
// NEGATIVE-NOT: @interface NSObject
// NEGATIVE-NOT: @class NSObject
class MyObject : NSObject {}
