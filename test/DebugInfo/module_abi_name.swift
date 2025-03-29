// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -module-name=Hello -module-abi-name Goodbye -emit-ir -o - | %FileCheck %s

class SomeClass {}
// CHECK: DICompositeType(tag: DW_TAG_structure_type, name: "SomeClass",{{.*}}runtimeLang: DW_LANG_Swift, identifier: "$s7Goodbye9SomeClassCD"

let v1 = SomeClass()

