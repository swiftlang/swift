// RUN: %target-swift-ide-test -print-module -module-to-print=Classes -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK-NOT: extension
// CHECK: extension ClassesNS1.ClassesNS2 {
// CHECK:   struct BasicStruct {
// CHECK:     init()
// CHECK:     mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK:   }
// CHECK:   struct ForwardDeclaredStruct {
// CHECK:     init()
// CHECK:     mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK:   }
// CHECK: }
// CHECK-NOT: extension

// CHECK: extension ClassesNS1 {
// CHECK:   struct BasicStruct {
// CHECK:     init()
// CHECK:     mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK:   }
// CHECK:   struct ForwardDeclaredStruct {
// CHECK:     init()
// CHECK:     mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK:   }
// CHECK: }
// CHECK-NOT: extension

// CHECK: extension ClassesNS3 {
// CHECK:   struct BasicStruct {
// CHECK:     init()
// CHECK:     mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK:   }
// CHECK: }
// CHECK-NOT: extension
