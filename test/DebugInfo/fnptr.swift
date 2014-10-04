// RUN: %swift -target x86_64-apple-macosx10.9 %s -emit-ir -g -o - | FileCheck %s

// CHECK-DAG: ![[SINODE:.*]] = {{.*}} null, null, metadata ![[SI:.*]]} ; [ DW_TAG_structure_type ] [Int]
// CHECK-DAG: ![[SFNODE:.*]] = {{.*}} null, null, metadata ![[SF:.*]]} ; [ DW_TAG_structure_type ] [Float]
// CHECK-DAG: ![[VOIDNODE:.*]] = {{.*}} null, null, metadata ![[VOID:.*]]} ; [ DW_TAG_structure_type ] [_TtT_]
func bar() {
    print ("bar()")
}
func baz(i: Float) -> Int { return 0; }
func barz(i: Float, j: Float) -> Int { return 0; }
func main() -> Int {

    // CHECK-DAG: metadata !"[[BARPT:[^,]+]]"} ; [ DW_TAG_auto_variable ] [bar_function_pointer] [line [[@LINE+1]]]
    var bar_function_pointer = bar
    // CHECK-DAG: \0030"{{, [^,]+, [^,]+, [^,]+}}, metadata ![[BARMEMBERS:[0-9]+]]{{.*}} ; [ DW_TAG_structure_type ] {{.*}}[[BARPT]]
    // CHECK-DAG: ![[BARMEMBERS]] = metadata !{metadata ![[BARMEMBER:.*]]}
    // CHECK-DAG: ![[BARMEMBER]] = {{.*}}metadata ![[BARPTR:[0-9]+]]} ; [ DW_TAG_member ] [pointer]
    // CHECK-DAG: ![[BARPTR]] = {{.*}} metadata ![[BART:[0-9]+]]} ; [ DW_TAG_pointer_type ]
    // CHECK-DAG: ![[BART]] = {{.*}}, metadata ![[BARARGS:[0-9]+]], {{.*}}} ; [ DW_TAG_subroutine_type ]
    // CHECK-DAG: ![[BARARGS]] = metadata !{metadata !"_TtT_"}
    bar_function_pointer();// Set breakpoint here

    // CHECK-DAG: metadata !"[[BAZPT:[^,]+]]"} ; [ DW_TAG_auto_variable ] [baz_function_pointer]
    // CHECK-DAG: \0030"{{, [^,]+, [^,]+, [^,]+}}, metadata ![[BAZMEMBERS:[0-9]+]], {{.*}}} ; [ DW_TAG_structure_type ] {{.*}}[[BAZPT]]
    // CHECK-DAG: ![[BAZMEMBERS]] = metadata !{metadata ![[BAZMEMBER:.*]]}
    // CHECK-DAG: ![[BAZMEMBER]] = {{.*}}metadata ![[BAZPTR:[0-9]+]]} ; [ DW_TAG_member ] [pointer]
    // CHECK-DAG: ![[BAZPTR]] = {{.*}} metadata ![[BAZT:[0-9]+]]} ; [ DW_TAG_pointer_type ]
    // CHECK-DAG: ![[BAZT]] = {{.*}}metadata ![[BAZARGS:.*]], null, null, null} ; [ DW_TAG_subroutine_type ]
    // CHECK-DAG: ![[BAZARGS]] = metadata !{metadata !"_TtSi", metadata !"_TtSf"}
    var baz_function_pointer = baz
    baz_function_pointer(2.89)

    // CHECK-DAG: metadata !"[[BARZPT:[^,]+]]"} ; [ DW_TAG_auto_variable ] [barz_function_pointer]
    // CHECK-DAG: \0030"{{, [^,]+, [^,]+, [^,]+}}, metadata ![[BARZMEMBERS:[0-9]+]], {{.*}}} ; [ DW_TAG_structure_type ] {{.*}}[[BARZPT]]
    // CHECK-DAG: ![[BARZMEMBERS]] = metadata !{metadata ![[BARZMEMBER:.*]]}
    // CHECK-DAG: ![[BARZMEMBER]] = {{.*}}metadata ![[BARZPTR:[0-9]+]]} ; [ DW_TAG_member ] [pointer]
    // CHECK-DAG: ![[BARZPTR]] = {{.*}} metadata ![[BARZT:[0-9]+]]} ; [ DW_TAG_pointer_type ]
    // CHECK-DAG: ![[BARZT]] = {{.*}}metadata ![[BARZARGS:.*]], null, null, null} ; [ DW_TAG_subroutine_type ]
    // CHECK-DAG: ![[BARZARGS]] = metadata !{metadata !"_TtSi", metadata !"_TtSf", metadata !"_TtSf"}
    var barz_function_pointer = barz
    return barz_function_pointer(2.89, -1.0)
}

main()
