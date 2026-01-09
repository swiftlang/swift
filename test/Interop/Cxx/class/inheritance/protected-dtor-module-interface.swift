// RUN: %target-swift-ide-test -print-module -module-to-print=ProtectedDtor -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s

// CHECK:      struct Derived {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var inDerived: Int32
// CHECK-NEXT: }
