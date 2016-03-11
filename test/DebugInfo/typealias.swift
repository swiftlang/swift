// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

class DWARF {
// CHECK-DAG: ![[DIEOFFSET:.*]] = !DIDerivedType(tag: DW_TAG_typedef, name: "_TtaC9typealias5DWARF9DIEOffset",{{.*}} line: [[@LINE+1]], baseType: !"_TtVs6UInt32")
  typealias DIEOffset = UInt32
  // CHECK-DAG: ![[PRIVATETYPE:.*]] = !DIDerivedType(tag: DW_TAG_typedef, name: "_TtaC9typealias5DWARFP{{.+}}11PrivateType",{{.*}} line: [[@LINE+1]], baseType: !"_TtT_")
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
