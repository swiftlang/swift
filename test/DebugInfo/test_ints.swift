// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s
//
// CHECK: [ DW_TAG_base_type ] [Builtin.Int64] [line {{.*}}, size 64, align 64

func test_ints() {
   var int8_minus_two  = Int8(-2)
   var int8_plus_two   = Int8(2)
   var int16_minus_two = Int16(-2)
   var int16_plus_two  = Int16(2)
   var int32_minus_two = Int32(-2)
   var int32_plus_two  = Int32(2)
   var int64_minus_two = Int64(-2)
   var int64_plus_two  = Int64(2)
   var int_minus_two   = Int(-2)
   var int_plus_two    = Int(2)

   var uint8_minus_two  = UInt8(-2)
   var uint8_plus_two   = UInt8(2)
   var uint16_minus_two = UInt16(-2)
   var uint16_plus_two  = UInt16(2)
   var uint32_minus_two = UInt32(-2)
   var uint32_plus_two  = UInt32(2)
   var uint64_minus_two = UInt64(-2)
   var uint64_plus_two  = UInt64(2)
}
