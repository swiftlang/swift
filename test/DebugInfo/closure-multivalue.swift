// RUN: %target-swift-frontend -Xllvm -debug-values-propagate-liveness -O %s -disable-llvm-optzns -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

// CHECK: define {{.*}}4main4demo
// CHECK: define {{.*}}hidden i1 {{.*}}4main4sort
// CHECK: call void @llvm.dbg.value(metadata i8*{{.*}}, metadata ![[A:.*]], metadata ![[P1:.*]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[A]], metadata ![[P2:.*]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[A]], metadata ![[P3:.*]])
// CHECK: call void @llvm.dbg.value(metadata i8*{{.*}}, metadata ![[B:.*]], metadata ![[P1]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[B]], metadata ![[P2]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[B]], metadata ![[P3]])
// CHECK-DAG: ![[A]] = !DILocalVariable({{.*}} name: "a",{{.*}} line: 18
// CHECK-DAG: ![[B]] = !DILocalVariable({{.*}} name: "b",{{.*}} line: 18
// CHECK-DAG: ![[P1]] = !DIExpression(DW_OP_bit_piece, 0, {{(32|64)}})
// CHECK-DAG: ![[P2]] = !DIExpression(DW_OP_bit_piece, {{(32, 32|64, 64)}})
// CHECK-DAG: ![[P3]] = !DIExpression(DW_OP_bit_piece, {{(64, 32|128, 64)}})
public func sort(a: String, b: String) -> Bool {
  markUsed("Sorting..\(a) & \(b)")
  return (a < b)
}

public func demo() {
    var names = ["Sean", "Barry", "Kate"]
    var sortedNames = sorted(names, sort)
    var sortedNamesAsString : String = String()
    for name in sortedNames {
        sortedNamesAsString += ("\(name), ")
    }
    markUsed(sortedNamesAsString)
}
demo()

// At -O0, we should have a single aggregate argument.
// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s --check-prefix=CHECK-O0
// Verify that a reabstraction thunk does not have a line number.
// CHECK-O0-NOT: DW_OP_bit_piece
// CHECK-O0: !DISubprogram(linkageName: "_TTRXFo_oSSoSS_dSb_XFo_iSSiSS_dSb_", scope: !{{[0-9]+}}, file: !{{[0-9]+}}, type: !{{[0-9]+}},
// CHECK-O0-NOT: DW_OP_bit_piece
// CHECK-O0: !DILocalVariable(tag: DW_TAG_arg_variable, name: "a",{{.*}} line: 18,
// CHECK-O0-NOT: DW_OP_bit_piece
// CHECK-O0: !DILocalVariable(tag: DW_TAG_arg_variable, name: "b",{{.*}} line: 18,
// CHECK-O0-NOT: DW_OP_bit_piece
