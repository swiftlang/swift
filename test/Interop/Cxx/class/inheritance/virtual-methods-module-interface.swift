// RUN: %target-swift-ide-test -print-module -print-implicit-attrs -module-to-print=VirtualMethods -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct Base {
// CHECK-NEXT:  init()
// CHECK-NEXT:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK-NEXT:   mutating func foo()

// CHECK: struct Derived<CInt> {
// CHECK-NEXT:  init()
// CHECK-NEXT:  mutating func foo()
// CHECK: }

// CHECK: struct VirtualNonAbstractBase {
// CHECK-NEXT:  init()
// CHECK-NEXT:  func nonAbstractMethod()
