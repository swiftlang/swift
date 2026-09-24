// RUN: %target-swift-ide-test -print-module -module-to-print=BitFields -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s

// Bit-fields are imported as computed properties backed by synthesized
// getters and setters.

// CHECK:      struct BitFields {
// CHECK-NEXT:   init(a: CUnsignedInt, b: CUnsignedInt, c: CUnsignedInt)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var a: CUnsignedInt
// CHECK-NEXT:   var b: CUnsignedInt
// CHECK-NEXT:   var c: CUnsignedInt
// CHECK-NEXT: }
