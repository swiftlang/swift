// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

class DWARF {
// CHECK-DAG: ![[BASE:.*]] = !DICompositeType({{.*}}identifier: "$ss6UInt32VD"
// CHECK-DAG: ![[DIEOFFSET:.*]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s9typealias5DWARFC9DIEOffsetaD",{{.*}} line: [[@LINE+1]], baseType: ![[BASE]])
  typealias DIEOffset = UInt32
  // CHECK-DAG: ![[VOID:.*]] = !DICompositeType({{.*}}identifier: "$sytD"
  // CHECK-DAG: ![[PRIVATETYPE:.*]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s9typealias5DWARFC11PrivateType{{.+}}aD",{{.*}} line: [[@LINE+1]], baseType: ![[VOID]])
  fileprivate typealias PrivateType = ()
  fileprivate static func usePrivateType() -> PrivateType { return () }
}

struct Generic<T> {
  enum Inner {
    case value
  }
}

typealias Specific = Generic<Int>
typealias NotSpecific<T> = Generic<T>.Inner

struct Outer : P {
  enum Inner {
    case x
  }
}

protocol P {
  typealias T = Outer
}

// CHECK-DAG: [[INNER_TYPE:![0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type{{.*}} identifier: "$s9typealias5OuterV5InnerOD"

func main () {
  // CHECK-DAG: !DILocalVariable(name: "a",{{.*}} type: ![[DIEOFFSET]]
  let a : DWARF.DIEOffset = 123
  markUsed(a)
  // CHECK-DAG: !DILocalVariable(name: "b",{{.*}} type: ![[DIEOFFSET]]
  let b = DWARF.DIEOffset(456) as DWARF.DIEOffset
  markUsed(b)

  // CHECK-DAG: !DILocalVariable(name: "c",{{.*}} type: ![[PRIVATETYPE]]
  let c = DWARF.usePrivateType()
  markUsed(c);

  // CHECK-DAG: !DILocalVariable(name: "d", {{.*}} type: [[NONGENERIC_TYPE:![0-9]+]])
  // CHECK: [[NONGENERIC_TYPE]] = !DICompositeType(tag: DW_TAG_structure_type{{.*}} identifier: "$s9typealias7GenericV5InnerOySi_GD"
  let d: Specific.Inner = .value
  markUsed(d)

  // CHECK-DAG: !DILocalVariable(name: "e", {{.*}} type: [[INNER_TYPE]])
  let e: Outer.T.Inner = .x
  markUsed(e)

  // CHECK-DAG: !DILocalVariable(name: "f", {{.*}} type: [[OUTER_T_TYPE:![0-9]+]])
  // CHECK: [[OUTER_T_TYPE]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s9typealias1PP1TayAA5OuterV_GD"
  let f: Outer.T = Outer()
  markUsed(f)

  // CHECK-DAG: !DILocalVariable(name: "g", {{.*}} type: [[GENERIC_TYPE:![0-9]+]])
  // CHECK: [[GENERIC_TYPE]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s9typealias11NotSpecificaySiGD"
  let g: NotSpecific<Int> = .value
  markUsed(g)
}

main()
