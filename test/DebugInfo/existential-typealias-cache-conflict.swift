// RUN: %target-build-swift -emit-ir -module-name LayeredCache -g %s -o - | %FileCheck %s

protocol P<Value> {
    associatedtype Value
}

actor A<Value> {
    public typealias T = P<Value>
    let t: any T
    init(i: Int, t: any T) {
        self.init(t: t)
    }
    init(t: any T) {
        self.t = t
    }
}

// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "t", {{.*}}, baseType: ![[EXIST_TY:[0-9]+]]
// CHECK-DAG: ![[EXIST_TY]] = !DICompositeType(tag: DW_TAG_structure_type, name: "P", {{.*}}, elements: ![[ELTS:[0-9]+]], {{.*}}, identifier: "$s12LayeredCache1P_px5ValueAaBPRts_XPD")
// CHECK-DAG: ![[ELTS]] = !{![[INNER:[0-9]+]]}
// CHECK-DAG: ![[INNER]] = !DIDerivedType(tag: DW_TAG_member, name: "$swift.constraint", {{.*}}, baseType: ![[TYPEDEF:[0-9]+]]
// CHECK-DAG: ![[TYPEDEF]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s12LayeredCache1AC1Tayx_GD", {{.*}}, baseType: ![[PROTO_TY:[0-9]+]])
// CHECK-DAG: ![[PROTO_TY]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s12LayeredCache1P_px5ValueAaBPRts_XPD", {{.*}}, identifier: "$s12LayeredCache1P_px5ValueAaBPRts_XPD")
