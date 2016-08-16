// RUN: %target-swift-frontend -primary-file %s -emit-ir -gdwarf-types -o - | %FileCheck %s

// Verify that the size of a class that has not been created before
// its outer type is emitted is emitted correctly.
public struct S { var x : Int64; var y : Int64}
public enum Enum {
  case WithClass(C)
  case WithStruct(S)
}
public class C { }

public let e : Enum = .WithClass(C())

// CHECK: !DIDerivedType(tag: DW_TAG_member, name: "WithClass",
// CHECK-SAME:           size: {{32|64}},
// CHECK: !DIDerivedType(tag: DW_TAG_member, name: "WithStruct",
// CHECK-SAME:           size: 128,

