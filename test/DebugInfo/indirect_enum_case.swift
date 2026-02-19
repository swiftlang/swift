// RUN: %target-swift-frontend -primary-file %s -emit-ir -gdwarf-types -o - | %FileCheck %s

// Test that explicit `indirect case` declarations are represented with
// DW_TAG_reference_type in debug info, while non-indirect cases
// are represented directly.

// CHECK: !DICompositeType(tag: DW_TAG_variant_part, {{.*}}elements: ![[ELTS:[0-9]+]])
// CHECK: ![[ELTS]] = !{![[EMPTY:[0-9]+]], ![[VALUE:[0-9]+]], ![[INDIRECT:[0-9]+]]}

// The empty case has no type
// CHECK: ![[EMPTY]] = !DIDerivedType(tag: DW_TAG_member, name: "empty",

// The value case should NOT have a reference type (it's not indirect)
// CHECK: ![[VALUE]] = !DIDerivedType(tag: DW_TAG_member, name: "value", {{.*}}baseType: ![[INT:[0-9]+]]
// CHECK: ![[INT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int",

// The indirect case should have a reference type
// CHECK: ![[INDIRECT]] = !DIDerivedType(tag: DW_TAG_member, name: "indirectValue", {{.*}}baseType: ![[IND_REF:[0-9]+]]
// CHECK: ![[IND_REF]] = !DIDerivedType(tag: DW_TAG_reference_type, baseType: ![[INT]]

enum MixedEnum {
  case empty
  case value(Int)
  indirect case indirectValue(Int)
}

let mixed = MixedEnum.value(42)
