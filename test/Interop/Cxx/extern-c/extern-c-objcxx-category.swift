// RUN: %target-swift-ide-test -print-module -module-to-print=ExternC -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-objc-interop | %FileCheck %s

// CHECK: class A {
// CHECK-NEXT: }

// CHECK: extension A {
// CHECK-NEXT:   class func foo() -> Int32
// CHECK-NEXT:   func foo() -> Int32
// CHECK-NEXT: }

// CHECK: extension A {
// CHECK-NEXT:   class func bar() -> Int32
// CHECK-NEXT:   func bar() -> Int32
// CHECK-NEXT: }
