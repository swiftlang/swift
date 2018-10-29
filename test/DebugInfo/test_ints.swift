// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

// These two should not have the same type.
// CHECK: !DIGlobalVariable(name: "a",{{.*}} line: [[@LINE+2]]
// CHECK-SAME:              type: ![[INT64:[0-9]+]]
var a : Int64 = 2
// CHECK: ![[INT64]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int64"
// CHECK-SAME:             size: 64,
// CHECK-NOT:              offset: 0
// CHECK-NOT:              DIFlagFwdDecl
// CHECK-SAME:             identifier: "$ss5Int64VD"

// CHECK: !DIGlobalVariable(name: "b",{{.*}} line: [[@LINE+2]]
// CHECK-SAME:              type: ![[INT:[0-9]+]]
var b = 2
// CHECK: ![[INT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "Int"
// CHECK-SAME:                        identifier: "$sSiD"

