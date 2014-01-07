// This test deliberately does not go through a module to better test ordering.

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: swift2objc %clang-importer-sdk -module-cache-path=%t/clang-module-cache %s -o %t/protocols.h
// RUN: FileCheck %s < %t/protocols.h
// RUN: FileCheck --check-prefix=NEGATIVE %s < %t/protocols.h

// CHECK: @import swift;
// CHECK-NEXT: @import Foundation;
import Foundation

// CHECK: @protocol A{{$}}
// CHECK-NEXT: @end
@objc @class_protocol protocol A {}

// CHECK: @protocol B <A>
// CHECK-NEXT: @end
@objc @class_protocol protocol B : A {}

// CHECK: @protocol A2{{$}}
// CHECK-NEXT: @end
// CHECK: @protocol B2 <A2>
// CHECK-NEXT: @end
@objc @class_protocol protocol B2 : A2 {}
@objc @class_protocol protocol A2 {}

// NEGATIVE-NOT: NotObjC
@class_protocol protocol NotObjC {}

// CHECK: @interface RootClass1{{$}}
// CHECK: @interface RootClass2 <A>{{$}}
// CHECK: @interface RootClass3 <B>{{$}}
@objc class RootClass1 : NotObjC {}
@objc class RootClass2 : A, NotObjC {}
@objc class RootClass3 : NotObjC, B {}

// CHECK: @protocol C{{$}}
// CHECK: @interface RootClass4 <A, C>{{$}}
@objc class RootClass4 : A, C {}

// CHECK: @interface Subclass : RootClass1 <C>{{$}}
@objc class Subclass : RootClass1, C {}

// CHECK: @interface MyObject : NSObject <NSCoding>
// CHECK-NEXT: @end
// NEGATIVE-NOT: @protocol NSCoding
class MyObject : NSObject, NSCoding {}


// CHECK: @interface NSString (){{$}}
extension NSString : NotObjC {}

// CHECK: @interface NSString () <A, C>
extension NSString : A, C {}


// Deliberately at the end of the file.
@objc @class_protocol protocol C {}
