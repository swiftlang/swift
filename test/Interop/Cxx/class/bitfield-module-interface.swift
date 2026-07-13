// RUN: %target-swift-ide-test -print-module -module-to-print=BitFields -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s

// Bit-fields are imported as computed properties backed by synthesized
// getters and setters.

// CHECK:      struct BitFields {
// CHECK-NEXT:   init(a: UInt32, b: UInt32, c: UInt32)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var a: UInt32
// CHECK-NEXT:   var b: UInt32
// CHECK-NEXT:   var c: UInt32
// CHECK-NEXT: }
