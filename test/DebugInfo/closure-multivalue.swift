// RUN: %swift -target x86_64-apple-macosx10.9 -O %s -emit-ir -g -o - | FileCheck %s
// CHECK-DAG: ![[A:.*]] = {{.*}} [ DW_TAG_arg_variable ] [a] [line 15]
// CHECK-DAG: ![[B:.*]] = {{.*}} [ DW_TAG_arg_variable ] [b] [line 15]
// CHECK-DAG: ![[P1:.*]] = {{.*}}; [ DW_TAG_expression ] [DW_OP_piece offset=0, size=8]
// CHECK-DAG: ![[P2:.*]] = {{.*}}; [ DW_TAG_expression ] [DW_OP_piece offset=8, size=8]
// CHECK-DAG: ![[P3:.*]] = {{.*}}; [ DW_TAG_expression ] [DW_OP_piece offset=16, size=8]
// CHECK-DAG: call void @llvm.dbg.value({{.*}}, metadata ![[A]], metadata ![[P1]])
// CHECK-DAG: call void @llvm.dbg.value({{.*}}, metadata ![[A]], metadata ![[P2]])
// CHECK-DAG: call void @llvm.dbg.value({{.*}}, metadata ![[A]], metadata ![[P3]])
// CHECK-DAG: call void @llvm.dbg.value({{.*}}, metadata ![[B]], metadata ![[P1]])
// CHECK-DAG: call void @llvm.dbg.value({{.*}}, metadata ![[B]], metadata ![[P2]])
// CHECK-DAG: call void @llvm.dbg.value({{.*}}, metadata ![[B]], metadata ![[P3]])
func demo () {
    var names = ["Sean", "Barry", "Kate"]
    var sortedNames = sorted(names) {(a, b) in
        println("Sorting..\(a) & \(b)")
        return (a < b)
    }
    var sortedNamesAsString : String = String()
    for name in sortedNames {
        sortedNamesAsString += ("\(name), ")
    }
    println(sortedNamesAsString)
}
demo()

// At -O0, we should have a single aggregate argument.
// RUN: %swift -target x86_64-apple-macosx10.9 %s -emit-ir -g -o - | FileCheck %s --check-prefix=CHECK-O0
// CHECK-O0: [ DW_TAG_arg_variable ] [a] [line 15]
// CHECK-O0-NOT: piece
// CHECK-O0: [ DW_TAG_arg_variable ] [b] [line 15]
// CHECK-O0-NOT: piece
