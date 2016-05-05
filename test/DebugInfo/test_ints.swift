// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// These two should not have the same type.

// CHECK: !DIGlobalVariable(name: "a", {{.*}}, line: [[@LINE+6]], type: ![[ATY:[0-9]+]]
// CHECK: ![[ATY]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int64"
// CHECK-SAME:             size: 64, align: 64
// CHECK-NOT:              offset: 0
// CHECK-NOT:              DIFlagFwdDecl
// CHECK-SAME:             identifier: "_TtVs5Int64"
// CHECK: !DIGlobalVariable(name: "a",{{.*}} line: [[@LINE+2]]
// CHECK-SAME:              type: !"_TtVs5Int64"
var a : Int64 = 2

// CHECK: !DIGlobalVariable(name: "b", {{.*}}, line: [[@LINE+2]], type: ![[BTY:[0-9]+]]
// CHECK: ![[BTY]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", {{.*}}, identifier: "_TtSi")
var b = 2

