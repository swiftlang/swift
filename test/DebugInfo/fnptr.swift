// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -g -o - | FileCheck %s

// CHECK-DAG: ![[SINODE:.*]] = {{.*}} null, null, metadata ![[SI:.*]]} ; [ DW_TAG_structure_type ] [Int]
// CHECK-DAG: ![[SFNODE:.*]] = {{.*}} null, null, metadata ![[SF:.*]]} ; [ DW_TAG_structure_type ] [Float]
// CHECK-DAG: ![[VOIDNODE:.*]] = {{.*}} null, null, metadata ![[VOID:.*]]} ; [ DW_TAG_structure_type ] [_TtT_]
func bar() {
    print ("bar()")
}
func baz(i: Float) -> Int { return 0; }
func barz(i: Float, j: Float) -> Int { return 0; }
func main() -> Int {

  // CHECK-DAG: i32 {{.*}}, metadata ![[BARPT:.*]], i32 0, i32 0, i64 2} ; [ DW_TAG_auto_variable ] [bar_function_pointer] [line [[@LINE+1]]]
    var bar_function_pointer = bar
  // CHECK-DAG: [[BARPT]]{{.*}}, metadata ![[BARMEMBERS:[0-9]+]], i32 40960,{{.*}} ; [ DW_TAG_structure_type ] [_TtFT_T_]
    // CHECK-DAG: ![[BARMEMBERS]] = metadata !{metadata ![[BARMEMBER:.*]]}
    // CHECK-DAG: ![[BARMEMBER]] = {{.*}}metadata ![[BARPTR:[0-9]+]]} ; [ DW_TAG_member ] [pointer]
    // CHECK-DAG: ![[BARPTR]] = {{.*}} metadata ![[BART:[0-9]+]]} ; [ DW_TAG_pointer_type ]
    // CHECK-DAG: ![[BART]] = {{.*}}null, metadata ![[BARARGS:.*]], i32 0, null, null, null} ; [ DW_TAG_subroutine_type ]
    // CHECK-DAG: ![[BARARGS]] = metadata !{metadata ![[VOIDNODE]]}
    bar_function_pointer();// Set breakpoint here

    // CHECK-DAG: i32 {{.*}}, metadata ![[BAZPT:.*]], i32 0, i32 0, i64 2} ; [ DW_TAG_auto_variable ] [baz_function_pointer]
    // CHECK-DAG: [[BAZPT]]{{.*}}, metadata ![[BAZMEMBERS:[0-9]+]], i32 40960,{{.*}} ; [ DW_TAG_structure_type ] [_TtFSfSi]
    // CHECK-DAG: ![[BAZMEMBERS]] = metadata !{metadata ![[BAZMEMBER:.*]]}
    // CHECK-DAG: ![[BAZMEMBER]] = {{.*}}metadata ![[BAZPTR:[0-9]+]]} ; [ DW_TAG_member ] [pointer]
    // CHECK-DAG: ![[BAZPTR]] = {{.*}} metadata ![[BAZT:[0-9]+]]} ; [ DW_TAG_pointer_type ]
    // CHECK-DAG: ![[BAZT]] = {{.*}}metadata ![[BAZARGS:.*]], i32 0, null, null, null} ; [ DW_TAG_subroutine_type ]
    // CHECK-DAG: ![[BAZARGS]] = metadata !{metadata ![[SINODE]], metadata ![[SFNODE]]}
    var baz_function_pointer = baz
    baz_function_pointer(2.89)

    // CHECK-DAG: i32 {{.*}}, metadata ![[BARZPT:.*]], i32 0, i32 0, i64 2} ; [ DW_TAG_auto_variable ] [barz_function_pointer] 
    // CHECK-DAG: [[BARZPT]]{{.*}}, metadata ![[BARZMEMBERS:[0-9]+]], i32 40960,{{.*}} ; [ DW_TAG_structure_type ] [_TtFTSfSf_Si]
    // CHECK-DAG: ![[BARZMEMBERS]] = metadata !{metadata ![[BARZMEMBER:.*]]}
    // CHECK-DAG: ![[BARZMEMBER]] = {{.*}}metadata ![[BARZPTR:[0-9]+]]} ; [ DW_TAG_member ] [pointer]
    // CHECK-DAG: ![[BARZPTR]] = {{.*}} metadata ![[BARZT:[0-9]+]]} ; [ DW_TAG_pointer_type ]
    // CHECK-DAG: ![[BARZT]] = {{.*}}metadata ![[BARZARGS:.*]], i32 0, null, null, null} ; [ DW_TAG_subroutine_type ]
    // CHECK-DAG: ![[BARZARGS]] = metadata !{metadata ![[SINODE]], metadata ![[SFNODE]], metadata ![[SFNODE]]}
    var barz_function_pointer = barz
    return barz_function_pointer(2.89, -1.0)
}

main()
