// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -g -o - | FileCheck %s
//

func test_ints() {
   var int8_minus_two : Int8  = -2
   var int8_plus_two  : Int8  = 2
   var int16_minus_two : Int16 = -2
   var int16_plus_two  : Int16 = 2
   var int32_minus_two : Int32 = -2
   var int32_plus_two  : Int32 = 2
   var int64_minus_two : Int64 = -2
   var int64_plus_two  : Int64 = 2
   var int_minus_two   = -2
   var int_plus_two    = 2

   var uint8_plus_two  : UInt8 = 2
   var uint16_plus_two : UInt16 = 2
   var uint32_plus_two : UInt32 = 2
   var uint64_plus_two : UInt64 = 2
}

// rdar://problem/15078795
// These two should not have the same type.
// CHECK: [ DW_TAG_base_type ] [_TtBi64_] [line 0, size 64, align 64
// CHECK: metadata !"a", {{.*}}, i32 [[@LINE+1]], metadata ![[INT64:.*]], i32 0
var a : Int64 = 2

// CHECK: metadata !"b",{{.*}}, i32 [[@LINE+3]],
// CHECK-NOT: metadata ![[INT64]]
// CHECK: i32 0, i32 1
var b = 2

