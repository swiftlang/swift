// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -module-name main -o - | %FileCheck %s

// Mangling a retroactive conditional conformance in a type alias's
// substitutions requires a generic signature, even when the variable's
// sugared type contains no archetypes.

extension Comparable {
  public typealias Alias = Int
}

extension Optional: @retroactive Comparable where Wrapped: Comparable {
  public static func < (lhs: Self, rhs: Self) -> Bool { false }

  public func f(_ x: Alias) {
    let y: Self.Alias = x
    _ = y
  }
}

// CHECK-DAG: !DILocalVariable(name: "x", arg: 1,{{.*}} type: ![[CONST:[0-9]+]]
// CHECK-DAG: ![[CONST]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[ALIAS:[0-9]+]])
// CHECK-DAG: ![[ALIAS]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$sSL4mainE5AliasayxSg_ADSLAAxSLHD1__HCg_GD"
// CHECK-DAG: !DILocalVariable(name: "y",{{.*}} type: ![[CONST]]
