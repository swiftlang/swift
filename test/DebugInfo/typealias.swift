// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(_ t: T) {}

class DWARF {
  // CHECK-DAG: ![[DIEOFFSET:[0-9]+]] = !DIDerivedType(tag: DW_TAG_typedef, name: "_TtaC9typealias5DWARF9DIEOffset", {{.*}}, line: [[@LINE+2]], baseType: ![[DIE_OFFSET_BASETYPE:[0-9]+]])
  // CHECK-DAG: ![[DIE_OFFSET_BASETYPE]] = !DICompositeType(tag: DW_TAG_structure_type, name: "UInt32", {{.*}}, identifier: "_TtVs6UInt32")
  typealias DIEOffset = UInt32

  // CHECK-DAG: ![[PRIVATETYPE:[0-9]+]] = !DIDerivedType(tag: DW_TAG_typedef, name: "_TtaC9typealias5DWARFP{{.+}}11PrivateType",{{.*}} line: [[@LINE+2]], baseType: ![[PRIVATETYPE_BASETYPE:[0-9]+]])
  // CHECK-DAG: ![[PRIVATETYPE_BASETYPE]] = !DICompositeType(tag: DW_TAG_structure_type, name: "_TtT_", {{.*}}, identifier: "_TtT_")
  private typealias PrivateType = ()
  private static func usePrivateType() -> PrivateType { return () }
}

func main () {
  // CHECK-DAG: !DILocalVariable(name: "a",{{.*}} type: ![[DIEOFFSET]]
  var a : DWARF.DIEOffset = 123
  markUsed(a)
  // CHECK-DAG: !DILocalVariable(name: "b",{{.*}} type: ![[DIEOFFSET]]
  var b = DWARF.DIEOffset(456) as DWARF.DIEOffset
  markUsed(b)
  
  // CHECK-DAG: !DILocalVariable(name: "c",{{.*}} type: ![[PRIVATETYPE]]
  let c = DWARF.usePrivateType()
}

main();

