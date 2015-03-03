// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// CHECK-DAG: ![[SINODE:.*]] = !MDCompositeType(tag: DW_TAG_structure_type, name: "Int",{{.*}} identifier: [[SI:.*]])
// CHECK-DAG: ![[SFNODE:.*]] = !MDCompositeType(tag: DW_TAG_structure_type, name: "Float",{{.*}} identifier: [[SF:.*]])
// CHECK-DAG: ![[VOIDNODE:.*]] = !MDCompositeType(tag: DW_TAG_structure_type, name: "_TtT_",{{.*}} identifier: [[VOID:.*]])
func bar() {
    print ("bar()")
}
func baz(i: Float) -> Int { return 0; }
func barz(i: Float, j: Float) -> Int { return 0; }
func main() -> Int {

    // CHECK-DAG: !MDLocalVariable(tag: DW_TAG_auto_variable, name: "bar_function_pointer",{{.*}} line: [[@LINE+1]],{{.*}} type: !"[[BARPT:[^,]+]]"
    var bar_function_pointer = bar
    // CHECK-DAG: !MDCompositeType(tag: DW_TAG_structure_type, name: "[[BARPT]]",{{.*}} elements: ![[BARMEMBERS:[0-9]+]]
    // CHECK-DAG: ![[BARMEMBERS]] = !{![[BARMEMBER:.*]]}
    // CHECK-DAG: ![[BARMEMBER]] = !MDDerivedType(tag: DW_TAG_member,{{.*}} baseType: ![[BARPTR:[0-9]+]]
    // CHECK-DAG: ![[BARPTR]] = !MDDerivedType(tag: DW_TAG_pointer_type,{{.*}} baseType: ![[BART:[0-9]+]]
    // CHECK-DAG: ![[BART]] = !MDSubroutineType(types: ![[BARARGS:[0-9]+]])
    // CHECK-DAG: ![[BARARGS]] = !{!"_TtT_"}
    bar_function_pointer();// Set breakpoint here

    // CHECK-DAG: !MDLocalVariable(tag: DW_TAG_auto_variable, name: "baz_function_pointer",{{.*}} type: !"[[BAZPT:[^,]+]]"
    // CHECK-DAG: !MDCompositeType(tag: DW_TAG_structure_type, name: "[[BAZPT]]",{{.*}} elements: ![[BAZMEMBERS:[0-9]+]]
    // CHECK-DAG: ![[BAZMEMBERS]] = !{![[BAZMEMBER:.*]]}
    // CHECK-DAG: ![[BAZMEMBER]] = !MDDerivedType(tag: DW_TAG_member,{{.*}} baseType: ![[BAZPTR:[0-9]+]]
    // CHECK-DAG: ![[BAZPTR]] = !MDDerivedType(tag: DW_TAG_pointer_type,{{.*}} baseType: ![[BAZT:[0-9]+]]
    // CHECK-DAG: ![[BAZT]] = !MDSubroutineType(types: ![[BAZARGS:.*]])
    // CHECK-DAG: ![[BAZARGS]] = !{!"_TtSi", !"_TtSf"}
    var baz_function_pointer = baz
    baz_function_pointer(2.89)

    // CHECK-DAG: !MDLocalVariable(tag: DW_TAG_auto_variable, name: "barz_function_pointer",{{.*}} type: !"[[BARZPT:[^,]+]]"
    // CHECK-DAG: !MDCompositeType(tag: DW_TAG_structure_type, name: "[[BARZPT]]",{{.*}} elements: ![[BARZMEMBERS:[0-9]+]]
    // CHECK-DAG: ![[BARZMEMBERS]] = !{![[BARZMEMBER:.*]]}
    // CHECK-DAG: ![[BARZMEMBER]] = !MDDerivedType(tag: DW_TAG_member,{{.*}} baseType: ![[BARZPTR:[0-9]+]]
    // CHECK-DAG: ![[BARZPTR]] = !MDDerivedType(tag: DW_TAG_pointer_type,{{.*}} baseType: ![[BARZT:[0-9]+]]
    // CHECK-DAG: ![[BARZT]] = !MDSubroutineType(types: ![[BARZARGS:.*]])
    // CHECK-DAG: ![[BARZARGS]] = !{!"_TtSi", !"_TtSf", !"_TtSf"}
    var barz_function_pointer = barz
    return barz_function_pointer(2.89, -1.0)
}

main()
