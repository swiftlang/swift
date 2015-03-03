// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// These two should not have the same type.
// CHECK: !MDCompositeType(tag: DW_TAG_structure_type, name: "Int64"
// CHECK-SAME:             size: 64, align: 64
// CHECK-NOT:              offset: 0
// CHECK-NOT:              DIFlagFwdDecl
// CHECK-SAME:             identifier: "_TtVSs5Int64"
// CHECK: !MDGlobalVariable(name: "a",{{.*}} line: [[@LINE+2]]
// CHECK-SAME:              type: !"_TtVSs5Int64"
var a : Int64 = 2

// CHECK: !MDGlobalVariable(name: "b",{{.*}} line: [[@LINE+2]]
// CHECK-SAME:              type: !"_TtSi"
var b = 2

