// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t/modules) -Xcc -DCGFLOAT_IN_COREFOUNDATION -DCGFLOAT_IN_COREFOUNDATION -emit-module -o %t/modules/CoreFoundation.swiftmodule %clang-importer-sdk-path/swift-modules/CoreFoundation.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t/modules) -Xcc -DCGFLOAT_IN_COREFOUNDATION -DCGFLOAT_IN_COREFOUNDATION -emit-module -o %t/modules/CoreGraphics.swiftmodule %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t/modules) -Xcc -DCGFLOAT_IN_COREFOUNDATION -DCGFLOAT_IN_COREFOUNDATION -emit-module -o %t/modules/Foundation.swiftmodule %clang-importer-sdk-path/swift-modules/Foundation.swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t/modules) %t/src/main.swift \
// RUN:   -import-bridging-header %t/src/test.h \
// RUN:   -module-name main -I %t -emit-sil | %FileCheck %s

// REQUIRES: objc_interop

//--- test.h
#include <objc/objc.h>

@import CoreFoundation;
@import CoreGraphics;
@import Foundation;

@interface MyClass : NSObject
@end

__attribute__((swift_name("MyClass.value")))
static const int MyClassValue = -1;

static const CGFloat myFloatConstValue = 42.0;

typedef CGFloat MyFloatType __attribute__((swift_wrapper(struct)));
static const MyFloatType MyFloatTypeValue1 = 10;
static const MyFloatType MyFloatTypeValue2 = 20;
static const MyFloatType MyFloatTypeValue3 = 30;

//--- main.swift
func foo() {
  print(MyClass.value)
  print(myFloatConstValue)
  print(MyFloatType.value1)
  print(MyFloatType.value2)
  print(MyFloatType.value3)
}

// CHECK:      // static MyClass.value.getter
// CHECK-NEXT: // Isolation: nonisolated
// CHECK-NEXT: sil shared [transparent] @$sSo7MyClassC5values5Int32VvgZ : $@convention(method) (@thick MyClass.Type) -> Int32 {
// CHECK-NEXT: // %0 "self"
// CHECK-NEXT: bb0(%0 : $@thick MyClass.Type):
// CHECK-NEXT:   debug_value %0, let, name "self", argno 1
// CHECK-NEXT:   %2 = integer_literal $Builtin.Int32, -1
// CHECK-NEXT:   %3 = struct $Int32 (%2)
// CHECK-NEXT:   return %3
// CHECK-NEXT: }

// CGFloats are not imported:

// CHECK-NOT: // myFloatConstValue.getter
// CHECK-NOT: // static MyFloatType.value1.getter
// CHECK-NOT: // static MyFloatType.value2.getter
// CHECK-NOT: // static MyFloatType.value3.getter
