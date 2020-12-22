// RUN: %target-swift-ide-test -print-module -module-to-print=NestedRecords -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct S1 {
// CHECK:   struct S2 {
// CHECK:     var A: Bool
// CHECK:   }
// CHECK: }

// CHECK: struct S3 {
// CHECK:   struct S4 {
// CHECK:   }
// CHECK: }

// CHECK: struct U1 {
// CHECK:   struct U2 {
// CHECK:   }
// CHECK: }
 
// CHECK: struct U3 {
// CHECK:   struct E1 : Equatable, RawRepresentable {
// CHECK:     typealias RawValue = {{UInt32|Int32}}
// CHECK:   }
// CHECK: }
 
// CHECK: struct U4 {
// CHECK:   struct S5 {
// CHECK:   }
// CHECK: }
 
// CHECK: struct S6 {
// CHECK:   struct E3 : Equatable, RawRepresentable {
// CHECK:     typealias RawValue = {{UInt32|Int32}}
// CHECK:   }
// CHECK:   init()
// CHECK: }
 
// CHECK: struct S7 {
// CHECK:   struct U5 {
// CHECK:     struct U6 {
// CHECK:     }
// CHECK:   }
// CHECK: }
 
// CHECK: struct S8 {
// CHECK:   struct S9 {
// CHECK:     struct U7 {
// CHECK:     }
// CHECK:   }
// CHECK: }
 
// CHECK: struct S10 {
// CHECK:   struct U8 {
// CHECK:     struct E4 : Equatable, RawRepresentable {
// CHECK:       typealias RawValue = {{UInt32|Int32}}
// CHECK:     }
// CHECK:   }
// CHECK: }

// CHECK: struct HasForwardDeclaredNestedType {
// CHECK:   struct NormalSubType {
// CHECK:     init()
// CHECK:   }
// CHECK:   struct ForwardDeclaredType {
// CHECK:     init()
// CHECK:   }
// CHECK:   init()
// CHECK: }
