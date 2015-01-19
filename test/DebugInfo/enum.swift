// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// CHECK: [ DW_TAG_union_type ] [_TtO4enum5Color] [line [[@LINE+1]], size 8, align 8,
enum Color : UInt {
// CHECK: [ DW_TAG_member ] [Red] [line 0, size 8, align 8, offset 0] [from _TtSu]
  case Red, Green, Blue
}

// CHECK: [ DW_TAG_union_type ] [_TtO4enum12MaybeIntPair] [line [[@LINE+1]], size 136, align 64,
enum MaybeIntPair {
// CHECK: [ DW_TAG_member ] [None] [line 0, size 136, align 64, offset 0] [from _TtSi]
case None
// CHECK: [ DW_TAG_member ] [Just] [line 0, size 136, align 64, offset 0] [from _TtTVSs5Int64S__]
  case Just(Int64, Int64)
}

enum Maybe<T> {
  case None
  case Just(T)
}

let r = Color.Red
let c = MaybeIntPair.Just(74, 75)
// CHECK: [ DW_TAG_union_type ] [_TtGO4enum5MaybeOS_5Color_] [line [[@LINE-7]], size 8, align 8
let movie : Maybe<Color> = .None
