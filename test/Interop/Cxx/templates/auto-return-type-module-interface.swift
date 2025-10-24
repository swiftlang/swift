// RUN: %target-swift-ide-test -print-module -module-to-print=AutoReturnType -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK-NOT: func canNotDeduce(_ a: Int32, _ b: Int32)

// CHECK: struct HasMethodReturningAuto<CInt> {
// CHECK:   func getT() -> Int32
// CHECK:   func getPtrT() -> UnsafePointer<Int32>!
// CHECK:   func getConstant() -> Double
// CHECK:   func outOfLineDefinition() -> Int32
// CHECK: }
