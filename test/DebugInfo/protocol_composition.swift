// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

protocol P1 {}
protocol P2 {}

func f() {
  let x: any P1 & P2
}

// The protocol composition type should have DW_TAG_inheritance members
// referencing each constituent protocol.
// CHECK-DAG: ![[COMP:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s20protocol_composition2P1_AA2P2pD"{{.*}}elements: ![[ELTS:[0-9]+]]
// CHECK-DAG: ![[ELTS]] = !{![[INH1:[0-9]+]], ![[INH2:[0-9]+]]}
// CHECK-DAG: ![[INH1]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[COMP]], baseType: ![[P1:[0-9]+]]
// CHECK-DAG: ![[INH2]] = !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[COMP]], baseType: ![[P2:[0-9]+]]
// CHECK-DAG: ![[P1]] = !DICompositeType(tag: DW_TAG_structure_type, name: "P1"
// CHECK-DAG: ![[P2]] = !DICompositeType(tag: DW_TAG_structure_type, name: "P2"
