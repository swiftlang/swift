// RUN: %target-swift-ide-test -print-module -module-to-print=ClassesSecondHeader -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: extension ClassesNS1.ClassesNS2 {
// CHECK-NOT: extension
// CHECK:   struct DefinedInDefs {
// CHECK:     init()
// CHECK:     mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK:   }
// CHECK: }
