// RUN: %target-swift-frontend -primary-file %s -emit-ir -gdwarf-types -o - | %FileCheck %s

protocol TraitsProtocol {
  associatedtype IntType: FixedWidthInteger
}

final class MyClass<SomeTraits: TraitsProtocol> {
  typealias Traits = SomeTraits
  private var blame: MyClass<Traits>?
}

struct Traits32: TraitsProtocol {
  typealias IntType = UInt32
}

let v : MyClass<Traits32>? = nil


// This typedef is not necessary from a semantic point of view, but the result
// of a trade-off done to preserve type sugar (= not generally canonicalizing types).
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_typedef, name: "$s4main7MyClassCyAC6TraitsayAA8Traits32V_GGSgD", {{.*}}baseType: ![[CANONICAL:[0-9]+]])
// CHECK-DAG: ![[CANONICAL]] = !DICompositeType(tag: DW_TAG_structure_type, {{.*}}identifier: "$s4main7MyClassCyAA8Traits32VGSgD", specification: ![[OPTIONAL:[0-9]+]])

// CHECK-DAG: ![[OPTIONAL]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Optional"

// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "MyClass", {{.*}}templateParams: ![[PARAMS:[0-9]+]], identifier: "$s4main7MyClassCyAA8Traits32VGD", specification: ![[SPEC:[0-9]+]])
// CHECK-DAG: ![[PARAMS]] = !{![[PARAM:[0-9]+]]}
// CHECK-DAG: ![[PARAM]] = !DITemplateTypeParameter(type: ![[TRAITS32:[0-9]+]])
// CHECK-DAG: ![[TRAITS32]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Traits32"
// CHECK-DAG: ![[SPEC]] = !DICompositeType(tag: DW_TAG_structure_type, name: "MyClass", {{.*}}identifier: "$s4main7MyClassCyxGD")


// have a second cache from (DIScope???, CanonicalMangledName) -> DIType
// if is non-canonical type emit a typealias to DIType.

struct C {}
struct A {
  typealias B = C
  let b: B
}

struct D {
  typealias B = C
  let b: B
}

let a : A? = nil
let b : D? = nil

// Check that the mechanism creating the canoicalization typedefs, doesn't merge
// more than it should:
// CHECK-DAG: ![[B1:[0-9]+]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s4main1AV1BaD"
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "b", {{.*}} baseType: ![[B1]])
// CHECK-DAG: ![[B2:[0-9]+]] = !DIDerivedType(tag: DW_TAG_typedef, name: "$s4main1DV1BaD"
// CHECK-DAG: !DIDerivedType(tag: DW_TAG_member, name: "b", {{.*}}, baseType: ![[B2]])
