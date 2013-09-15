// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s

// CHECK-DAG: ![[SI:.*]] = {{.*}}[ DW_TAG_structure_type ] [_TtSi]
// CHECK-DAG: ![[SF:.*]] = {{.*}}[ DW_TAG_structure_type ] [_TtSf]
// CHECK-DAG: ![[VOID:.*]] = {{.*}}[ DW_TAG_structure_type ] [_TtT_]
func bar() {
    print ("bar()")
}
func baz(i: Float) -> Int { return 0; }
func barz(i: Float, j: Float) -> Int { return 0; }
func main() -> Int {

    // CHECK-DAG: i32 {{.*}}, metadata ![[BARPTR:.*]], i32 0, i32 0} ; [ DW_TAG_auto_variable ] [bar_function_pointer] 
    // CHECK-DAG: ![[BARPTR]] ={{.*}}metadata ![[BART:.*]]} ; [ DW_TAG_pointer_type ]
    // CHECK-DAG: ![[BART]] = {{.*}}null, metadata ![[BARARGS:.*]], i32 0, null, null, null} ; [ DW_TAG_subroutine_type ]
    // CHECK-DAG: ![[BARARGS]] = metadata !{metadata ![[VOID]]}
    var bar_function_pointer = bar
    bar_function_pointer();// Set breakpoint here

    // CHECK-DAG: i32 {{.*}}, metadata ![[BAZPTR:.*]], i32 0, i32 0} ; [ DW_TAG_auto_variable ] [baz_function_pointer] 
    // CHECK-DAG: ![[BAZPTR]] ={{.*}}metadata ![[BAZT:.*]]} ; [ DW_TAG_pointer_type ]
    // CHECK-DAG: ![[BAZT]] = {{.*}}metadata ![[BAZARGS:.*]], i32 0, null, null, null} ; [ DW_TAG_subroutine_type ]
    // CHECK-DAG: ![[BAZARGS]] = metadata !{metadata ![[SI]], metadata ![[SF]]}
    var baz_function_pointer = baz
    baz_function_pointer(2.89)

    // CHECK-DAG: i32 {{.*}}, metadata ![[BARZPTR:.*]], i32 0, i32 0} ; [ DW_TAG_auto_variable ] [barz_function_pointer] 
    // CHECK-DAG: ![[BARZPTR]] ={{.*}}metadata ![[BARZT:.*]]} ; [ DW_TAG_pointer_type ]
    // CHECK-DAG: ![[BARZT]] = {{.*}}metadata ![[BARZARGS:.*]], i32 0, null, null, null} ; [ DW_TAG_subroutine_type ]
    // CHECK-DAG: ![[BARZARGS]] = metadata !{metadata ![[SI]], metadata ![[SF]], metadata ![[SF]]}
    var barz_function_pointer = barz
    return barz_function_pointer(2.89, -1.0)
}

main()
