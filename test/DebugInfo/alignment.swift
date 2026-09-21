// RUN: %target-swift-frontend %s -emit-ir -gdwarf-types -o - | %FileCheck %s

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

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "MultiPayload"
// CHECK-SAME:             size: 40, align: 32,
enum MultiPayload {
  case a(Int32)
  case b(Bool)
  case c
}


// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "SameAlignAsSize"
// CHECK-SAME:             size: 64, align: 64,
struct SameAlignAsSize { var x: Int64 }

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "Empty"
// CHECK-NOT: size:
// CHECK-SAME: align: 8,
struct Empty {}

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "ZeroSizedPayload"
// CHECK-NOT: size:
// CHECK-SAME: align: 8,
enum ZeroSizedPayload { case a(Empty) }

var m: MultiPayload
var a: SameAlignAsSize
var t: Empty
var z: ZeroSizedPayload
