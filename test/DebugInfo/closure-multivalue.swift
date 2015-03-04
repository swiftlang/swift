// RUN: %target-swift-frontend -Xllvm -debug-values-propagate-liveness -O %s -disable-llvm-optzns -emit-ir -g -o - | FileCheck %s
// CHECK: define {{.*}}4main4demo
// CHECK: define {{.*}}hidden i1 {{.*}}4main4sort
// CHECK: call void @llvm.dbg.value(metadata i8*{{.*}}, metadata ![[A:.*]], metadata ![[P1:.*]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[A]], metadata ![[P2:.*]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[A]], metadata ![[P3:.*]])
// CHECK: call void @llvm.dbg.value(metadata i8*{{.*}}, metadata ![[B:.*]], metadata ![[P1]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[B]], metadata ![[P2]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[B]], metadata ![[P3]])
// CHECK-DAG: ![[A]] = !MDLocalVariable({{.*}} name: "a",{{.*}} line: 15
// CHECK-DAG: ![[B]] = !MDLocalVariable({{.*}} name: "b",{{.*}} line: 15
// CHECK-DAG: ![[P1]] = !MDExpression(DW_OP_bit_piece, 0, {{(32|64)}})
// CHECK-DAG: ![[P2]] = !MDExpression(DW_OP_bit_piece, {{(32, 32|64, 64)}})
// CHECK-DAG: ![[P3]] = !MDExpression(DW_OP_bit_piece, {{(64, 32|128, 64)}})
func sort(a : String, b : String) -> Bool {
  println("Sorting..\(a) & \(b)")
  return (a < b)
}

func demo() {
    var names = ["Sean", "Barry", "Kate"]
    var sortedNames = sorted(names, sort)
    var sortedNamesAsString : String = String()
    for name in sortedNames {
        sortedNamesAsString += ("\(name), ")
    }
    println(sortedNamesAsString)
}
demo()

// At -O0, we should have a single aggregate argument.
// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s --check-prefix=CHECK-O0
// Verify that a reabstraction thunk does not have a line number.
// CHECK-O0-NOT: DW_OP_bit_piece
// CHECK-O0: !MDSubprogram({{.*}}linkageName: "_TTRXFo_oSSoSS_dSb_XFo_iSSiSS_dSb_"
// CHECK-O0-NOT: line:
// CHECK-O0-SAME: ){{$}}
// CHECK-O0-NOT: DW_OP_bit_piece
// CHECK-O0: !MDLocalVariable(tag: DW_TAG_arg_variable, name: "a",{{.*}} line: 15,
// CHECK-O0-NOT: DW_OP_bit_piece
// CHECK-O0: !MDLocalVariable(tag: DW_TAG_arg_variable, name: "b",{{.*}} line: 15,
// CHECK-O0-NOT: DW_OP_bit_piece
