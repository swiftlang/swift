// RUN: %target-swift-ide-test -print-module -module-to-print=FrtSequence \
// RUN:   -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default \
// RUN:   | %FileCheck %s

// FRT types are imported as Swift classes.
// CHECK: class ImmortalNode {
// CHECK:   var value: Int32
// CHECK: }

// CHECK: class ImmortalNode2 {
// CHECK:   var value: Int32
// CHECK: }

// CHECK: class SharedNode {
// CHECK:   var value: Int32
// CHECK:   func retain()
// CHECK:   func release()
// CHECK: }

// CHECK: func makeImmortalPtrVector()
// CHECK: func makeSharedPtrVector()
// CHECK: func makeImmortalValVector()

// CHECK: struct RawPtrIterContainer {
// CHECK:   func __beginUnsafe() -> ImmortalNode!
// CHECK:   func __endUnsafe() -> ImmortalNode!
// CHECK: }
