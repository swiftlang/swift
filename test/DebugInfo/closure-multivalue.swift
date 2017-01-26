// rdar://problem/23727705:
// RUN-DISABLED: %target-swift-frontend -O %s -disable-llvm-optzns -emit-ir -g -o - | %FileCheck %s
import StdlibUnittest

// CHECK: define {{.*}}i1 {{.*}}4main4sort
// CHECK: call void @llvm.dbg.value(metadata i8*{{.*}}, metadata ![[A:.*]], metadata ![[P1:.*]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[A]], metadata ![[P2:.*]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[A]], metadata ![[P3:.*]])
// CHECK: call void @llvm.dbg.value(metadata i8*{{.*}}, metadata ![[B:.*]], metadata ![[P1]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[B]], metadata ![[P2]])
// CHECK: call void @llvm.dbg.value(metadata i{{[0-9]+}} {{.*}}, metadata ![[B]], metadata ![[P3]])
// CHECK-DAG: ![[A]] = !DILocalVariable(name: "a",{{.*}} line: 17
// CHECK-DAG: ![[B]] = !DILocalVariable(name: "b",{{.*}} line: 17
// CHECK-DAG: ![[P1]] = !DIExpression(DW_OP_bit_piece, 0, {{(32|64)}})
// CHECK-DAG: ![[P2]] = !DIExpression(DW_OP_bit_piece, {{(32, 32|64, 64)}})
// CHECK-DAG: ![[P3]] = !DIExpression(DW_OP_bit_piece, {{(64, 32|128, 64)}})
public func sort(_ a: String, b: String) -> Bool {
  _blackHole("Sorting..\(a) & \(b)")
  return (a < b)
}

public func demo() {
    let names = ["Sean", "Barry", "Kate"]
    let sortedNames = names.sorted(by: sort)
    var sortedNamesAsString : String = String()
    for name in sortedNames {
        sortedNamesAsString += ("\(name), ")
    }
    _blackHole(sortedNamesAsString)
}
demo()

// At -O0, we should have a single aggregate argument.
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -emit-ir -g -o - | %FileCheck %s --check-prefix=CHECK-O0
// Verify that a reabstraction thunk does not have a line number.
// CHECK-O0-NOT: DW_OP_bit_piece
// CHECK-O0-NOT: DW_OP_bit_piece
// CHECK-O0: !DILocalVariable(name: "a", arg: 1{{.*}} line: 17,
// CHECK-O0-NOT: DW_OP_bit_piece
// CHECK-O0: !DILocalVariable(name: "b", arg: 2{{.*}} line: 17,
// CHECK-O0-NOT: DW_OP_bit_piece
// CHECK-O0: !DISubprogram(linkageName: "_T0SSSSSbIxxxd_SSSSSbIxiid_TR",
