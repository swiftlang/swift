// RUN: %target-swift-frontend -emit-ir -g %s | %FileCheck %s

public protocol P: ~Copyable {}

public typealias PP = P

// Make sure this doesn't crash:
public func withProto(_ e: borrowing any PP & ~Copyable) {}

// CHECK-DAG: ![[COMP:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s4main1P_pRi_s_XPD", size: {{[0-9]+}}, elements: ![[MEMBERS:[0-9]+]], runtimeLang: DW_LANG_Swift, identifier: "$s4main1P_pRi_s_XPD")
// CHECK-DAG: ![[MEMBERS]] = !{![[INH1:[0-9]+]], ![[INH2:[0-9]+]]}
// CHECK-DAG: ![[INH1]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[COMP]], baseType: ![[PP:[0-9]+]]
// CHECK-DAG: ![[PP]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s4main2PPaD"
// CHECK-DAG: ![[INH2]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[COMP]], baseType: ![[NC:[0-9]+]]
// CHECK-DAG: ![[NC]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$sypRi_s_XPD"
