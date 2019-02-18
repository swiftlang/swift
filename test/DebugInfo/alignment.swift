// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

@_alignment(8)
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "S8"
// CHECK-SAME:             align: 64,
struct S8 { var x, y, z, w: Float }

@_alignment(16)
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "E16"
// CHECK-SAME:             align: 128,
enum E16 {
  case F(Float)
  case I(Int64)
}

var s: S8
var e: E16
