// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

@_alignment(32)
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "S32"
// CHECK-SAME:             align: 256,
struct S32 { var x, y, z, w: Float }

@_alignment(16)
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "E16"
// CHECK-SAME:             align: 128,
enum E16 {
  case F(Float)
  case I(Int64)
}

var s: S32
var e: E16
