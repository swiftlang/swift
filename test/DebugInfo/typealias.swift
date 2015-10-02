// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

class DWARF {
// CHECK-DAG: ![[DIEOFFSET:.*]] = !DIDerivedType(tag: DW_TAG_typedef, name: "_TtaC9typealias5DWARF9DIEOffset",{{.*}} line: [[@LINE+1]], baseType: !"_TtVs6UInt32")
    typealias DIEOffset = UInt32
}

func main () {
  // CHECK-DAG: !DILocalVariable(name: "a",{{.*}} type: ![[DIEOFFSET]]
    var a : DWARF.DIEOffset = 123
    markUsed("a is \(a)")
  // CHECK-DAG: !DILocalVariable(name: "b",{{.*}} type: ![[DIEOFFSET]]
    var b = DWARF.DIEOffset(456) as DWARF.DIEOffset
    markUsed("b is \(b)")
}

main();
