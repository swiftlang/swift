// RUN: %target-swift-frontend %s -emit-ir -gdwarf-types -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s --check-prefix=AST

// CHECK-DAG: ![[SINODE:.*]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int64",{{.*}} identifier: [[SI:.*]])
// CHECK-DAG: ![[SFNODE:.*]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Float",{{.*}} identifier: [[SF:.*]])
// CHECK-DAG: ![[VOIDNODE:.*]] = !DICompositeType(tag: DW_TAG_structure_type, name: "_TtT_",{{.*}} identifier: [[VOID:.*]])
func bar() {
    print("bar()", terminator: "")
}
func baz(_ i: Float) -> Int64 { return 0; }
func barz(_ i: Float, _ j: Float) -> Int64 { return 0; }
func main() -> Int64 {
    // CHECK-DAG: !DILocalVariable(name: "bar_fnptr",{{.*}} line: [[@LINE+3]],{{.*}} type: ![[BARPT:[0-9]+]]
    // AST-DAG: !DILocalVariable(name: "bar_fnptr",{{.*}} line: [[@LINE+2]],{{.*}} type: ![[BAR_T:[0-9]+]]
    // AST-DAG: ![[BAR_T]] = !DICompositeType({{.*}}, identifier: "_TtXFo___")
    var bar_fnptr = bar
    // CHECK-DAG: ![[BARPT]] = !DICompositeType(tag: DW_TAG_structure_type, {{.*}} elements: ![[BARMEMBERS:[0-9]+]]
    // CHECK-DAG: ![[BARMEMBERS]] = !{![[BARMEMBER:.*]], {{.*}}}
    // CHECK-DAG: ![[BARMEMBER]] = !DIDerivedType(tag: DW_TAG_member,{{.*}} baseType: ![[BARPTR:[0-9]+]]
    // CHECK-DAG: ![[BARPTR]] = !DIDerivedType(tag: DW_TAG_pointer_type,{{.*}} baseType: ![[BART:[0-9]+]]
    // CHECK-DAG: ![[BART]] = !DISubroutineType(types: ![[BARARGS:[0-9]+]])
    // CHECK-DAG: ![[BARARGS]] = !{![[VOID:.*]]}
    // CHECK-DAG: ![[VOID]] = {{.*}}identifier: "_TtT_"
    bar_fnptr();

    // CHECK-DAG: !DILocalVariable(name: "baz_fnptr",{{.*}} type: ![[BAZPT:[0-9]+]]
    // AST-DAG: !DILocalVariable(name: "baz_fnptr",{{.*}} type: ![[BAZ_T:[0-9]+]]
    // AST-DAG: ![[BAZ_T]] = !DICompositeType({{.*}}, identifier: "_TtXFo_dSf_dVs5Int64_")
    // CHECK-DAG: ![[BAZPT]] = !DICompositeType(tag: DW_TAG_structure_type, {{.*}} elements: ![[BAZMEMBERS:[0-9]+]]
    // CHECK-DAG: ![[BAZMEMBERS]] = !{![[BAZMEMBER:.*]], {{.*}}}
    // CHECK-DAG: ![[BAZMEMBER]] = !DIDerivedType(tag: DW_TAG_member,{{.*}} baseType: ![[BAZPTR:[0-9]+]]
    // CHECK-DAG: ![[BAZPTR]] = !DIDerivedType(tag: DW_TAG_pointer_type,{{.*}} baseType: ![[BAZT:[0-9]+]]
    // CHECK-DAG: ![[BAZT]] = !DISubroutineType(types: ![[BAZARGS:.*]])
    // CHECK-DAG: ![[BAZARGS]] = !{![[INT:.*]], ![[FLOAT:.*]]}
    // CHECK-DAG: ![[INT]] = {{.*}}identifier: "_TtVs5Int64"
    // CHECK-DAG: ![[FLOAT]] = {{.*}}identifier: "_TtSf"
    var baz_fnptr = baz
    baz_fnptr(2.89)

    // CHECK-DAG: !DILocalVariable(name: "barz_fnptr",{{.*}} type: ![[BARZPT:[0-9]+]]
    // AST_DAG: !DILocalVariable(name: "barz_fnptr",{{.*}} type: ![[BARZ_T:[0-9]+]]
    // AST-DAG: ![[BARZ_T:[0-9]+]] = !DICompositeType({{.*}}, identifier: "_TtXFo_dSfdSf_dVs5Int64_")
    // CHECK-DAG: ![[BARZPT]] = !DICompositeType(tag: DW_TAG_structure_type,{{.*}} elements: ![[BARZMEMBERS:[0-9]+]]
    // CHECK-DAG: ![[BARZMEMBERS]] = !{![[BARZMEMBER:.*]], {{.*}}}
    // CHECK-DAG: ![[BARZMEMBER]] = !DIDerivedType(tag: DW_TAG_member,{{.*}} baseType: ![[BARZPTR:[0-9]+]]
    // CHECK-DAG: ![[BARZPTR]] = !DIDerivedType(tag: DW_TAG_pointer_type,{{.*}} baseType: ![[BARZT:[0-9]+]]
    // CHECK-DAG: ![[BARZT]] = !DISubroutineType(types: ![[BARZARGS:.*]])
    // CHECK-DAG: ![[BARZARGS]] = !{![[INT]], ![[FLOAT]], ![[FLOAT]]}
    var barz_fnptr = barz
    return barz_fnptr(2.89, -1.0)
}

main()
