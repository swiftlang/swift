// RUN: %target-swift-ide-test -print-module -module-to-print=Submodules.SubmoduleA -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=Submodules.SubmoduleB -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      enum NS1 {
// CHECK-NEXT:   enum NS2 {
// CHECK-NEXT:     struct BasicDeepA {
// CHECK-NEXT:       init()
// CHECK-NEXT:     }
// CHECK-NEXT:     struct BasicDeepB {
// CHECK-NEXT:       init()
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT:   struct BasicA {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT:   struct BasicB {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT: }
