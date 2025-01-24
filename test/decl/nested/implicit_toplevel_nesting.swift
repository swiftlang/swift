// RUN: %target-typecheck-verify-swift -parse-as-library -dump-ast | %FileCheck %s

struct Outer {}

actor Outer.InnerActor {}
// CHECK:      (extension_decl implicit "Outer"
// CHECK-NEXT:   (actor_decl {{.*}} "InnerActor" interface type="Outer.InnerActor.Type"

class Outer.InnerClass {}
// CHECK:      (extension_decl implicit "Outer"
// CHECK-NEXT:   (class_decl {{.*}} "InnerClass" interface type="Outer.InnerClass.Type"

enum Outer.InnerEnum {}
// CHECK:      (extension_decl implicit "Outer"
// CHECK-NEXT:   (enum_decl {{.*}} "InnerEnum" interface type="Outer.InnerEnum.Type"

protocol Outer.InnerProtocol {}
// CHECK:      (extension_decl implicit "Outer"
// CHECK-NEXT:   (protocol_decl {{.*}} "InnerProtocol" interface type="Outer.InnerProtocol.Type"

struct Outer.InnerStruct {}
// CHECK:      (extension_decl implicit "Outer"
// CHECK-NEXT:   (struct_decl {{.*}} "InnerStruct" interface type="Outer.InnerStruct.Type"

struct Outer.Middle {}
// CHECK:      (extension_decl implicit "Outer"
// CHECK-NEXT:   (struct_decl {{.*}} "Middle" interface type="Outer.Middle.Type"

struct Outer.Middle.Deepest {}
// CHECK:      (extension_decl implicit "Outer.Middle"
// CHECK-NEXT:   (struct_decl {{.*}} "Deepest" interface type="Outer.Middle.Deepest.Type"

struct [Int].Nested {}
// CHECK:      (extension_decl implicit "[Int]"
// CHECK-NEXT:   (struct_decl {{.*}} "Nested" interface type="Array<Element>.Nested.Type"

struct [Int].SubSequence.Nested {}
// CHECK:      (extension_decl implicit "Array<Int>.SubSequence"
// CHECK-NEXT:   (struct_decl {{.*}} "Nested" interface type="ArraySlice<Element>.Nested.Type"
