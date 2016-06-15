// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// CHECK-DAG: ![[SINODE:.*]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int64",{{.*}} identifier: [[SI:.*]])
// CHECK-DAG: ![[SFNODE:.*]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Float",{{.*}} identifier: [[SF:.*]])
// CHECK-DAG: ![[VOIDNODE:.*]] = !DICompositeType(tag: DW_TAG_structure_type, name: "_TtT_",{{.*}} identifier: [[VOID:.*]])
func bar() {
    print("bar()", terminator: "")
}
func baz(_ i: Float) -> Int64 { return 0; }
func barz(_ i: Float, _ j: Float) -> Int64 { return 0; }
func main() -> Int64 {

    // CHECK-DAG: !DILocalVariable(name: "bar_function_pointer",{{.*}} line: [[@LINE+1]],{{.*}} type: ![[BARPT:[0-9]+]]
    var bar_function_pointer = bar
    // CHECK-DAG: ![[BARPT]] = !DICompositeType(tag: DW_TAG_structure_type, {{.*}} elements: ![[BARMEMBERS:[0-9]+]]
    // CHECK-DAG: ![[BARMEMBERS]] = !{![[BARMEMBER:.*]], {{.*}}}
    // CHECK-DAG: ![[BARMEMBER]] = !DIDerivedType(tag: DW_TAG_member,{{.*}} baseType: ![[BARPTR:[0-9]+]]
    // CHECK-DAG: ![[BARPTR]] = !DIDerivedType(tag: DW_TAG_pointer_type,{{.*}} baseType: ![[BART:[0-9]+]]
    // CHECK-DAG: ![[BART]] = !DISubroutineType(types: ![[BARARGS:[0-9]+]])
    // CHECK-DAG: ![[BARARGS]] = !{![[VOID:.*]]}
    // CHECK-DAG: ![[VOID]] = {{.*}}identifier: "_TtT_"
    bar_function_pointer();// Set breakpoint here

    // CHECK-DAG: !DILocalVariable(name: "baz_function_pointer",{{.*}} type: ![[BAZPT:[0-9]+]]
    // CHECK-DAG: ![[BAZPT]] = !DICompositeType(tag: DW_TAG_structure_type, {{.*}} elements: ![[BAZMEMBERS:[0-9]+]]
    // CHECK-DAG: ![[BAZMEMBERS]] = !{![[BAZMEMBER:.*]], {{.*}}}
    // CHECK-DAG: ![[BAZMEMBER]] = !DIDerivedType(tag: DW_TAG_member,{{.*}} baseType: ![[BAZPTR:[0-9]+]]
    // CHECK-DAG: ![[BAZPTR]] = !DIDerivedType(tag: DW_TAG_pointer_type,{{.*}} baseType: ![[BAZT:[0-9]+]]
    // CHECK-DAG: ![[BAZT]] = !DISubroutineType(types: ![[BAZARGS:.*]])
    // CHECK-DAG: ![[BAZARGS]] = !{![[INT:.*]], ![[FLOAT:.*]]}
    // CHECK-DAG: ![[INT]] = {{.*}}identifier: "_TtVs5Int64"
    // CHECK-DAG: ![[FLOAT]] = {{.*}}identifier: "_TtSf"
    var baz_function_pointer = baz
    baz_function_pointer(2.89)

    // CHECK-DAG: !DILocalVariable(name: "barz_function_pointer",{{.*}} type: ![[BARZPT:[0-9]+]]
    // CHECK-DAG: ![[BARZPT]] = !DICompositeType(tag: DW_TAG_structure_type,{{.*}} elements: ![[BARZMEMBERS:[0-9]+]]
    // CHECK-DAG: ![[BARZMEMBERS]] = !{![[BARZMEMBER:.*]], {{.*}}}
    // CHECK-DAG: ![[BARZMEMBER]] = !DIDerivedType(tag: DW_TAG_member,{{.*}} baseType: ![[BARZPTR:[0-9]+]]
    // CHECK-DAG: ![[BARZPTR]] = !DIDerivedType(tag: DW_TAG_pointer_type,{{.*}} baseType: ![[BARZT:[0-9]+]]
    // CHECK-DAG: ![[BARZT]] = !DISubroutineType(types: ![[BARZARGS:.*]])
    // CHECK-DAG: ![[BARZARGS]] = !{![[INT]], ![[FLOAT]], ![[FLOAT]]}
    var barz_function_pointer = barz
    return barz_function_pointer(2.89, -1.0)
}

main()
