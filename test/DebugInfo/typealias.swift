// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

class DWARF
{
// CHECK-DAG: ![[DIEOFFSET:.*]] = !MDDerivedType(tag: DW_TAG_typedef, name: "_TtaC9typealias5DWARF9DIEOffset",{{.*}} line: [[@LINE+1]], baseType: !"_TtVSs6UInt32")
    typealias DIEOffset = UInt32
}

func main () {
  // CHECK-DAG: !MDLocalVariable(tag: DW_TAG_auto_variable, name: "a",{{.*}} type: ![[DIEOFFSET]]
    var a : DWARF.DIEOffset = 123
    println("a is \(a)")
  // CHECK-DAG: !MDLocalVariable(tag: DW_TAG_auto_variable, name: "b",{{.*}} type: ![[DIEOFFSET]]
    var b = DWARF.DIEOffset(456) as DWARF.DIEOffset
    println("b is \(b)")
}

main();
