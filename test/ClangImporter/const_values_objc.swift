// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/src/main.swift \
// RUN:   -import-bridging-header %t/src/test.h \
// RUN:   -module-name main -I %t -emit-sil | %FileCheck %s

// REQUIRES: objc_interop

//--- test.h
#include <objc/objc.h>

@interface MyClass : NSObject
@end

__attribute__((swift_name("MyClass.value")))
static const int MyClassValue = -1;

//--- main.swift
func foo() {
  print(MyClass.value)
}

// CHECK:      sil shared [transparent] @$sSo7MyClassC5values5Int32VvgZ : $@convention(method) (@thick MyClass.Type) -> Int32 {
// CHECK-NEXT: // %0 "self"
// CHECK-NEXT: bb0(%0 : $@thick MyClass.Type):
// CHECK-NEXT:   debug_value %0, let, name "self", argno 1
// CHECK-NEXT:   %2 = integer_literal $Builtin.Int32, -1
// CHECK-NEXT:   %3 = struct $Int32 (%2)
// CHECK-NEXT:   return %3
// CHECK-NEXT: }
