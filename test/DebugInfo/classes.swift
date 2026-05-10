// RUN: %target-swift-frontend -primary-file %s -emit-ir -gdwarf-types -o - | %FileCheck %s

class SomeClass {
  let first = 4
  let second = "Hello"
}

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "SomeClass", 
// CHECK-SAME: size: {{64|32}}, elements: 
// CHECK-SAME: runtimeLang: DW_LANG_Swift,{{.*}} identifier: "$s7classes9SomeClassCD")

// CHECK: !DIDerivedType(tag: DW_TAG_member, name: "first",
// CHECK-SAME: size: {{64|32}})
// CHECK: !DIDerivedType(tag: DW_TAG_member, name: "second", 
// CHECK-SAME: size: {{128|96}}, offset: {{64|32}})
