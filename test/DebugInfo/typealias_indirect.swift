// RUN: %target-swift-frontend %s -emit-ir -parse-as-library -module-name a -g -o - | %FileCheck %s

// FIXME: While we can preserve that ClassAlias = MyClass<LocalAlias, Bool>
//        and we can preserve that MyClass<LocalAlias, Bool> = MyClass<Bool, Bool>
//        we cannot preserve that LocalAlias = Bool.

// CHECK: !DIDerivedType(tag: DW_TAG_typedef, name: "$s1a10ClassAliasaD"{{.*}}baseType: ![[LOCAL_BOOLTY:[0-9]+]]
// CHECK: ![[LOCAL_BOOLTY]] = !DICompositeType(tag: DW_TAG_structure_type, {{.*}}elements: ![[ELTS:[0-9]+]], runtimeLang: DW_LANG_Swift, specification: ![[SPEC:[0-9]+]])
// CHECK-DAG: ![[ELTS]] = !{![[MEMBER:[0-9]+]]}
// CHECK-DAG: ![[MEMBER]] = !DIDerivedType(tag: DW_TAG_member, {{.*}}baseType: ![[UNSIZED:[0-9]+]]
// CHECK-DAG: ![[UNSIZED]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s1a7MyClassCyAA10LocalAliasaSbGD"
// CHECK-DAG: ![[SPEC]] = !DICompositeType(tag: DW_TAG_structure_type, name: "MyClass"{{.*}}identifier: "$s1a7MyClassCyxq_GD")

// FIXME: !DIDerivedType(tag: DW_TAG_typedef, name: "$s1a10LocalAliasaD", {{.*}}baseType: ![[BASETY:[0-9]+]]
// FIXME: ![[BASETY]]{{.*}}$sSbD
public class MyClass<A, B> {}
public typealias LocalAlias = Bool
public typealias ClassAlias = MyClass<LocalAlias, Bool>
public func use(cls: ClassAlias?) {}

