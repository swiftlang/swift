// RUN: %target-swift-frontend %s -emit-ir -g -parse-as-library -module-name main -o - \
// RUN:   | %FileCheck %s

public protocol Plain { func f() -> Int }
public protocol Param<U> { associatedtype U; func g() -> U }
public protocol ClassParam<U>: AnyObject { associatedtype U; func g() -> U }
@_marker public protocol Marker {}

public func consume(_ a: any Plain,
                    _ b: any Param<Int>,
                    _ c: any ClassParam<Int>,
                    _ d: any Param<Int> & Marker) {}

// A plain existential is named after its protocol.
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "Plain",{{.*}}identifier: "$s4main5Plain_pD")

// A parameterized existential keeps the parameterized mangling as its identifier, but
// takes its name from the protocol.
// CHECK-DAG: ![[PARAM:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Param",{{.*}}identifier: "$s4main5Param_pSi1UAaBPRts_XPD")

// A class-constrained parameterized existential also keeps the annotation.
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "ClassParam",{{.*}}identifier: "$s4main10ClassParam_pSi1UAaBPRts_XPD", annotations: ![[CP_ANNOTS:[0-9]+]])
// CHECK-DAG: ![[CP_ANNOTS]] = !{![[CP_FLAG:[0-9]+]]}
// CHECK-DAG: ![[CP_FLAG]] = !{!"swift.ClassConstrainedProtocol", i1 true}

// A composition is still named by its mangled name -- it has no single decl -- but each
// member it inherits from is a protocol DIE that must be named, whether or not that
// member is parameterized.
// CHECK-DAG: ![[COMP:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s4main6Marker{{.*}}Rts_XPD",{{.*}}elements:
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[COMP]], baseType: ![[PARAM]],
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_inheritance, scope: ![[COMP]], baseType: ![[MARKER:[0-9]+]],
// CHECK-DAG: ![[MARKER]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Marker",{{.*}}annotations: ![[M_ANNOTS:[0-9]+]])
// CHECK-DAG: ![[M_ANNOTS]] = !{![[M_FLAG:[0-9]+]]}
// CHECK-DAG: ![[M_FLAG]] = !{!"swift.MarkerProtocol", i1 true}
