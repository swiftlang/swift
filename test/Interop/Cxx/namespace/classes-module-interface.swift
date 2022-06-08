// RUN: %target-swift-ide-test -print-module -module-to-print=Classes -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      enum ClassesNS1 {
// CHECK-NEXT:   struct ForwardDeclaredStruct {
// CHECK-NEXT:     init()
// CHECK-NEXT:     mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK-NEXT:   }
// CHECK-NEXT:   enum ClassesNS2 {
// CHECK-NEXT:     struct BasicStruct {
// CHECK-NEXT:       init()
// CHECK-NEXT:       mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK-NEXT:     }
// CHECK-NEXT:     struct ForwardDeclaredStruct {
// CHECK-NEXT:       init()
// CHECK-NEXT:       mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT:   struct BasicStruct {
// CHECK-NEXT:     init()
// CHECK-NEXT:     mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: enum ClassesNS3 {
// CHECK-NEXT:   struct BasicStruct {
// CHECK-NEXT:     init()
// CHECK-NEXT:     mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK-NEXT:   }
// CHECK-NEXT: }
// CHECK-NEXT: typealias GlobalAliasToNS1 = ClassesNS1
// CHECK-NEXT: enum ClassesNS4 {
// CHECK-NEXT:   typealias AliasToGlobalNS1 = ClassesNS1
// CHECK-NEXT:   typealias AliasToGlobalNS2 = ClassesNS1.ClassesNS2
// CHECK-NEXT:   enum ClassesNS5 {
// CHECK-NEXT:     struct BasicStruct {
// CHECK-NEXT:       init()
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT:   typealias AliasToInnerNS5 = ClassesNS4.ClassesNS5
// CHECK-NEXT:   typealias AliasToNS2 = ClassesNS1.ClassesNS2
// CHECK-NEXT:   typealias AliasChainToNS1 = ClassesNS1
// CHECK-NEXT:   typealias AliasChainToNS2 = ClassesNS1.ClassesNS2
// CHECK-NEXT: }
// CHECK-NEXT: enum ClassesNS5 {
// CHECK-NEXT:   struct BasicStruct {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT:   typealias AliasToAnotherNS5 = ClassesNS4.ClassesNS5
// CHECK-NEXT:   enum ClassesNS5 {
// CHECK-NEXT:     struct BasicStruct {
// CHECK-NEXT:       init()
// CHECK-NEXT:     }
// CHECK-NEXT:     typealias AliasToNS5NS5 = ClassesNS5.ClassesNS5
// CHECK-NEXT:   }
// CHECK-NEXT:   typealias AliasToGlobalNS5 = ClassesNS5
// CHECK-NEXT:   typealias AliasToLocalNS5 = ClassesNS5.ClassesNS5
// CHECK-NEXT:   typealias AliasToNS5 = ClassesNS5.ClassesNS5
// CHECK-NEXT: }
