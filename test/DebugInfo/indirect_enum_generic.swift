// RUN: %target-swift-frontend -primary-file %s -emit-ir -gdwarf-types -o - | %FileCheck %s

// Test that indirect cases in generic enums are represented with
// DW_TAG_reference_type in debug info.


// CHECK-DAG: ![[TREE_VP:[0-9]+]] = !DICompositeType(tag: DW_TAG_variant_part, {{.*}}elements: ![[TREE_ELTS:[0-9]+]])
// CHECK-DAG: ![[TREE_ELTS]] = !{![[LEAF:[0-9]+]], ![[BRANCH:[0-9]+]]}

// Both cases should have reference types since the entire enum is indirect
// CHECK-DAG: ![[LEAF]] = !DIDerivedType(tag: DW_TAG_member, name: "leaf", {{.*}}baseType: ![[LEAF_REF:[0-9]+]]
// CHECK-DAG: ![[LEAF_REF]] = !DIDerivedType(tag: DW_TAG_reference_type,

// CHECK-DAG: ![[BRANCH]] = !DIDerivedType(tag: DW_TAG_member, name: "branch", {{.*}}baseType: ![[BRANCH_REF:[0-9]+]]
// CHECK-DAG: ![[BRANCH_REF]] = !DIDerivedType(tag: DW_TAG_reference_type,

indirect enum Tree<T> {
  case leaf(T)
  case branch(Tree<T>, Tree<T>)
}


// CHECK-DAG: ![[OPT_VP:[0-9]+]] = !DICompositeType(tag: DW_TAG_variant_part, {{.*}}elements: ![[OPT_ELTS:[0-9]+]])
// CHECK-DAG: ![[OPT_ELTS]] = !{![[NONE:[0-9]+]], ![[SOME:[0-9]+]], ![[REC:[0-9]+]]}

// The empty case has no type
// CHECK-DAG: ![[NONE]] = !DIDerivedType(tag: DW_TAG_member, name: "none",

// The `some` case should NOT have a reference type (it's not indirect)
// CHECK-DAG: ![[SOME]] = !DIDerivedType(tag: DW_TAG_member, name: "some", {{.*}}baseType: ![[SOME_TY:[0-9]+]]
// CHECK-DAG: ![[SOME_TY]] = !DICompositeType(tag: DW_TAG_structure_type,

// The `recursive` case should have a reference type
// CHECK-DAG: ![[REC]] = !DIDerivedType(tag: DW_TAG_member, name: "recursive", {{.*}}baseType: ![[REC_REF:[0-9]+]]
// CHECK-DAG: ![[REC_REF]] = !DIDerivedType(tag: DW_TAG_reference_type,

enum GenericOption<T> {
  case none
  case some(T)
  indirect case recursive(GenericOption<T>)
}

let tree = Tree<Int>.leaf(42)
let opt = GenericOption<String>.some("hello")
