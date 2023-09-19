// RUN: %target-swift-ide-test -print-module -print-implicit-attrs -module-to-print=VirtualMethods -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct Base {
// CHECK-NEXT:  init()
// CHECK-NEXT:   @available(*, unavailable, message: "virtual functions are not yet available in Swift")
// CHECK-NEXT:   mutating func foo()

// CHECK: struct Derived<CInt> {
// CHECK: @available(*, unavailable, message: "virtual functions are not yet available in Swift")
// CHECK:  mutating func foo()
// CHECK: }

// CHECK: struct VirtualNonAbstractBase {
// CHECK:  @available(*, unavailable, message: "virtual functions are not yet available in Swift")
// CHECK:  func nonAbstractMethod()
