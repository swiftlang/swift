// RUN: %target-swift-ide-test -print-module -module-to-print=NestedRecords -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

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
// CHECK:   struct E1 : Hashable, Equatable, RawRepresentable {
// CHECK:     typealias RawValue = {{UInt32|Int32}}
// CHECK:   }
// CHECK: }
 
// CHECK: struct U4 {
// CHECK:   struct S5 {
// CHECK:   }
// CHECK: }
 
// CHECK: struct S6 {
// CHECK:   init()
// CHECK:   struct E3 : Hashable, Equatable, RawRepresentable {
// CHECK:     typealias RawValue = {{UInt32|Int32}}
// CHECK:   }
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
// CHECK:     struct E4 : Hashable, Equatable, RawRepresentable {
// CHECK:       typealias RawValue = {{UInt32|Int32}}
// CHECK:     }
// CHECK:   }
// CHECK: }

// CHECK: struct HasForwardDeclaredNestedType {
// CHECK:   init()
// CHECK:   struct ForwardDeclaredType {
// CHECK:     init()
// CHECK:   }
// CHECK:   struct NormalSubType {
// CHECK:     init()
// CHECK:   }
// CHECK: }

// CHECK: struct HasForwardDeclaredTemplateChild {
// CHECK:   init()
// CHECK:   struct ForwardDeclaredClassTemplate<T> {
// CHECK:   }
// CHECK:   struct DeclaresForwardDeclaredClassTemplateFriend {
// CHECK:     init()
// CHECK:   }
// CHECK: }

// CHECK: enum NestedDeclIsAFirstForwardDeclaration {
// CHECK:   struct ForwardDeclaresFriend {
// CHECK:     init()
// CHECK:   }
// CHECK:   struct ForwardDeclaredFriend {
// CHECK:     init()
// CHECK:   }
// CHECK:   func takesFriend(_ f: NestedDeclIsAFirstForwardDeclaration.ForwardDeclaredFriend)
// CHECK:   struct HasNestedForwardDeclaration {
// CHECK:     init()
// CHECK:     struct IsNestedForwardDeclaration {
// CHECK:       init()
// CHECK:       init(a: Int32)
// CHECK:       var a: Int32
// CHECK:     }
// CHECK:   }
// CHECK:   static func takesHasNestedForwardDeclaration(_: NestedDeclIsAFirstForwardDeclaration.HasNestedForwardDeclaration)
// CHECK: }
