// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=swift-5.9 -print-implicit-attrs -module-to-print=VirtualMethods -I %S/Inputs -source-filename=x | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=swift-6 -print-implicit-attrs -module-to-print=VirtualMethods -I %S/Inputs -source-filename=x | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=upcoming-swift -print-implicit-attrs -module-to-print=VirtualMethods -I %S/Inputs -source-filename=x | %FileCheck %s

// CHECK: struct Base {
// CHECK-NEXT:   @available(*, unavailable, message: "constructors of abstract C++ classes are unavailable in Swift")
// CHECK-NEXT:   init()
// CHECK-NEXT:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK-NEXT:   mutating func foo()
// CHECK: }

// CHECK: struct Base3 {
// CHECK-NEXT:   init()
// CHECK: }

// CHECK: struct Derived2 {
// CHECK-NEXT:   init()
// CHECK: }

// CHECK: struct Derived<CInt> {
// CHECK-NEXT:  init()
// CHECK-NEXT:  mutating func foo()
// CHECK: }

// CHECK: struct VirtualNonAbstractBase {
// CHECK-NEXT:  init()
// CHECK-NEXT:  func nonAbstractMethod()

// CHECK: class ImmortalBase {
// CHECK:  func get42() -> Int32
// CHECK:  func getOverridden42() -> Int32
// CHECK: }
// CHECK: class Immortal {
// CHECK:  func getOverridden42() -> Int32
// CHECK:  func get42() -> Int32
// CHECK: }
