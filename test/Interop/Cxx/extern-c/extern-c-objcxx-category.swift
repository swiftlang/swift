// RUN: %target-swift-ide-test -print-module -module-to-print=ExternC -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-objc-interop | %FileCheck %s

// CHECK: class A {
// CHECK-NEXT: }

// CHECK: extension A {
// CHECK-NEXT:   class func foo() -> CInt
// CHECK-NEXT:   func foo() -> CInt
// CHECK-NEXT: }

// CHECK: extension A {
// CHECK-NEXT:   class func bar() -> CInt
// CHECK-NEXT:   func bar() -> CInt
// CHECK-NEXT: }
