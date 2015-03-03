// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// CHECK: !MDCompositeType(tag: DW_TAG_union_type, name: "_TtO4enum5Color"
// CHECK-SAME:             line: [[@LINE+2]]
// CHECK-SAME:             size: 8, align: 8,
enum Color : UInt {
// CHECK: !MDDerivedType(tag: DW_TAG_member, name: "Red"
// CHECK-SAME:           baseType: !"_TtSu"
// CHECK-SAME:           size: 8, align: 8{{[,)]}}
  case Red, Green, Blue
}

// CHECK: !MDCompositeType(tag: DW_TAG_union_type, name: "_TtO4enum12MaybeIntPair"
// CHECK-SAME:             line: [[@LINE+2]],
// CHECK-SAME:             size: 136, align: 64{{[,)]}}
enum MaybeIntPair {
// CHECK: !MDDerivedType(tag: DW_TAG_member, name: "None"
// CHECK-SAME:           baseType: !"_TtSi"
// CHECK-SAME:           size: 136, align: 64{{[,)]}}
case None
// CHECK: !MDDerivedType(tag: DW_TAG_member, name: "Just"
// CHECK-SAME:           baseType: !"_TtTVSs5Int64S__"
// CHECK-SAME:           size: 136, align: 64{{[,)]}}
  case Just(Int64, Int64)
}

enum Maybe<T> {
  case None
  case Just(T)
}

let r = Color.Red
let c = MaybeIntPair.Just(74, 75)
// CHECK: !MDCompositeType(tag: DW_TAG_union_type, name: "_TtGO4enum5MaybeOS_5Color_"
// CHECK-SAME:             line: [[@LINE-8]],
// CHECK-SAME:             size: 8, align: 8{{[,)]}}
let movie : Maybe<Color> = .None
