// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

// Test that a generic type alias is represented in the debug info.

public struct S<T> {
  public typealias Alias = (T, T)
  // CHECK: ![[T_T:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sx_xtD"
  public let member : Alias
  public func f(t : Alias) -> Alias { return t }
  // CHECK: !DILocalVariable(name: "t", arg: 1,{{.*}} line: [[@LINE-1]],
  // CHECK-SAME:             type: ![[C_ALIAS:[0-9]+]])
  // CHECK: ![[C_ALIAS]] = !DIDerivedType(tag: DW_TAG_const_type,
  // CHECK-SAME:                          baseType: ![[ALIAS:[0-9]+]])
  // CHECK: ![[ALIAS]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$
  // CHECK-SAME:                        baseType: ![[T_T]])

}

public let s = S<Int>(member: (4, 2))
