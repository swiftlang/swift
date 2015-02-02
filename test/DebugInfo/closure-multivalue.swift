// RUN: %target-swift-frontend -Xllvm -debug-values-propagate-liveness -O %s -disable-llvm-optzns -emit-ir -g -o - | FileCheck %s
// CHECK: define {{.*}}4main4demo
// CHECK: define {{.*}}hidden i1 {{.*}}4main4sort
// CHECK: call void @llvm.dbg.value(metadata i8*{{.*}}, metadata ![[A:.*]], metadata ![[P1:.*]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[A]], metadata ![[P2:.*]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[A]], metadata ![[P3:.*]])
// CHECK: call void @llvm.dbg.value(metadata i8*{{.*}}, metadata ![[B:.*]], metadata ![[P1]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[B]], metadata ![[P2]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[B]], metadata ![[P3]])
// CHECK-DAG: ![[A]] = {{.*}} [ DW_TAG_a{{.*}}_variable ] [a] [line 15]
// CHECK-DAG: ![[B]] = {{.*}} [ DW_TAG_a{{.*}}_variable ] [b] [line 15]
// CHECK-DAG: ![[P1]] = {{.*}}; [ DW_TAG_expression ] [DW_OP_piece offset=0, size={{(4|8)}}]
// CHECK-DAG: ![[P2]] = {{.*}}; [ DW_TAG_expression ] [DW_OP_piece offset={{(4|8)}}, size={{(4|8)}}]
// CHECK-DAG: ![[P3]] = {{.*}}; [ DW_TAG_expression ] [DW_OP_piece offset={{(8|16)}}, size={{(4|8)}}]
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
// CHECK-O0: _TTRXFo_oSSoSS_dSb_XFo_iSSiSS_dSb_{{.*}}; [ DW_TAG_subprogram ] [line 0]
// CHECK-O0: [ DW_TAG_arg_variable ] [a] [line 15]
// CHECK-O0-NOT: piece
// CHECK-O0: [ DW_TAG_arg_variable ] [b] [line 15]
// CHECK-O0-NOT: piece
