// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// CHECK-DAG: ![[SINODE:.*]] = {{.*}} null, null, ![[SI:.*]]} ; [ DW_TAG_structure_type ] [Int]
// CHECK-DAG: ![[SFNODE:.*]] = {{.*}} null, null, ![[SF:.*]]} ; [ DW_TAG_structure_type ] [Float]
// CHECK-DAG: ![[VOIDNODE:.*]] = {{.*}} null, null, ![[VOID:.*]]} ; [ DW_TAG_structure_type ] [_TtT_]
func bar() {
    print ("bar()")
}
func baz(i: Float) -> Int { return 0; }
func barz(i: Float, j: Float) -> Int { return 0; }
func main() -> Int {

    // CHECK-DAG: !"[[BARPT:[^,]+]]"} ; [ DW_TAG_auto_variable ] [bar_function_pointer] [line [[@LINE+1]]]
    var bar_function_pointer = bar
    // CHECK-DAG: \0030"{{, [^,]+, [^,]+, [^,]+}}, ![[BARMEMBERS:[0-9]+]]{{.*}} ; [ DW_TAG_structure_type ] {{.*}}[[BARPT]]
    // CHECK-DAG: ![[BARMEMBERS]] = !{![[BARMEMBER:.*]]}
    // CHECK-DAG: ![[BARMEMBER]] = {{.*}}![[BARPTR:[0-9]+]]} ; [ DW_TAG_member ] [pointer]
    // CHECK-DAG: ![[BARPTR]] = {{.*}} ![[BART:[0-9]+]]} ; [ DW_TAG_pointer_type ]
    // CHECK-DAG: ![[BART]] = {{.*}}, ![[BARARGS:[0-9]+]], {{.*}}} ; [ DW_TAG_subroutine_type ]
    // CHECK-DAG: ![[BARARGS]] = !{!"_TtT_"}
    bar_function_pointer();// Set breakpoint here

    // CHECK-DAG: !"[[BAZPT:[^,]+]]"} ; [ DW_TAG_auto_variable ] [baz_function_pointer]
    // CHECK-DAG: \0030"{{, [^,]+, [^,]+, [^,]+}}, ![[BAZMEMBERS:[0-9]+]], {{.*}}} ; [ DW_TAG_structure_type ] {{.*}}[[BAZPT]]
    // CHECK-DAG: ![[BAZMEMBERS]] = !{![[BAZMEMBER:.*]]}
    // CHECK-DAG: ![[BAZMEMBER]] = {{.*}}![[BAZPTR:[0-9]+]]} ; [ DW_TAG_member ] [pointer]
    // CHECK-DAG: ![[BAZPTR]] = {{.*}} ![[BAZT:[0-9]+]]} ; [ DW_TAG_pointer_type ]
    // CHECK-DAG: ![[BAZT]] = {{.*}}![[BAZARGS:.*]], null, null, null} ; [ DW_TAG_subroutine_type ]
    // CHECK-DAG: ![[BAZARGS]] = !{!"_TtSi", !"_TtSf"}
    var baz_function_pointer = baz
    baz_function_pointer(2.89)

    // CHECK-DAG: !"[[BARZPT:[^,]+]]"} ; [ DW_TAG_auto_variable ] [barz_function_pointer]
    // CHECK-DAG: \0030"{{, [^,]+, [^,]+, [^,]+}}, ![[BARZMEMBERS:[0-9]+]], {{.*}}} ; [ DW_TAG_structure_type ] {{.*}}[[BARZPT]]
    // CHECK-DAG: ![[BARZMEMBERS]] = !{![[BARZMEMBER:.*]]}
    // CHECK-DAG: ![[BARZMEMBER]] = {{.*}}![[BARZPTR:[0-9]+]]} ; [ DW_TAG_member ] [pointer]
    // CHECK-DAG: ![[BARZPTR]] = {{.*}} ![[BARZT:[0-9]+]]} ; [ DW_TAG_pointer_type ]
    // CHECK-DAG: ![[BARZT]] = {{.*}}![[BARZARGS:.*]], null, null, null} ; [ DW_TAG_subroutine_type ]
    // CHECK-DAG: ![[BARZARGS]] = !{!"_TtSi", !"_TtSf", !"_TtSf"}
    var barz_function_pointer = barz
    return barz_function_pointer(2.89, -1.0)
}

main()
