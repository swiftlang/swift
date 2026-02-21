// RUN: %target-swift-frontend -primary-file %s -emit-ir -gdwarf-types -o - | %FileCheck %s

// Test that indirect enum cases are represented with DW_TAG_reference_type.

// CHECK: !DICompositeType(tag: DW_TAG_variant_part, {{.*}}elements: ![[ELTS:[0-9]+]])
// CHECK: ![[ELTS]] = !{![[LEAF:[0-9]+]], ![[BRANCH:[0-9]+]]}

// Both cases should have reference types since this is an indirect enum
// CHECK: ![[LEAF]] = !DIDerivedType(tag: DW_TAG_member, name: "leaf", {{.*}}baseType: ![[LEAF_REF:[0-9]+]]
// CHECK: ![[LEAF_REF]] = !DIDerivedType(tag: DW_TAG_reference_type, baseType: ![[INT:[0-9]+]]
// CHECK: ![[INT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int",

// CHECK: ![[BRANCH]] = !DIDerivedType(tag: DW_TAG_member, name: "branch", {{.*}}baseType: ![[BRANCH_REF:[0-9]+]]
// CHECK: ![[BRANCH_REF]] = !DIDerivedType(tag: DW_TAG_reference_type, baseType: ![[TUPLE:[0-9]+]]
// CHECK: ![[TUPLE]] = !DICompositeType(tag: DW_TAG_structure_type, name: "$s13indirect_enum8TreeNodeO_ACtD",

indirect enum TreeNode {
  case leaf(Int)
  case branch(TreeNode, TreeNode)
}

let tree = TreeNode.leaf(42)
