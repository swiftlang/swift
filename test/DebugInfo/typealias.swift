// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

class DWARF
{
// CHECK-DAG: ![[DIEOFFSET:.*]] = {{.*}}[ DW_TAG_typedef ] [_TtaC9typealias5DWARF9DIEOffset] [line [[@LINE+1]], size 0, align 0, offset 0] [from _TtVSs6UInt32]
    typealias DIEOffset = UInt32
}

func main () {
  // CHECK-DAG: ![[DIEOFFSET]]} ; [ DW_TAG_auto_variable ] [a] [line [[@LINE+1]]]
    var a : DWARF.DIEOffset = 123
    println("a is \(a)")
  // CHECK-DAG: ![[DIEOFFSET]]} ; [ DW_TAG_auto_variable ] [b] [line [[@LINE+1]]]
    var b = DWARF.DIEOffset(456) as DWARF.DIEOffset
    println("b is \(b)")
}

main();
