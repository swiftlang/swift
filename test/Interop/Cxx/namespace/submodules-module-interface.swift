// RUN: %target-swift-ide-test -print-module -module-to-print=Submodules.SubmoduleA -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=Submodules.SubmoduleB -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK:     extension NS1 {
// CHECK-NEXT:   struct BasicA {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: extension NS1 {
// CHECK-NEXT:   struct BasicB {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: extension NS1.NS2 {
// CHECK-NEXT:   struct BasicDeepA {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: extension NS1.NS2 {
// CHECK-NEXT:   struct BasicDeepB {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT: }
