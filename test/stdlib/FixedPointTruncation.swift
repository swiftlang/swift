// RUN: %swift -i %s
// REQUIRES: swift_interpreter

// Unsigned to signed truncation.
var maxInt8_u16 : UInt16 = 127
Int8(maxInt8_u16)
var maxInt8_u64 : UInt64 = 127
Int8(maxInt8_u64)
var zero_u16 : UInt16 = 0
Int8(zero_u16)

// Unsigned to unsigned truncation.
var maxUInt16_u32 : UInt32 = 0xFFFF
var small_u32 : UInt32 = 0xF
UInt16(maxUInt16_u32)
UInt16(zero_u16)
UInt16(small_u32)

// Signed to unsigned truncation.
var maxUInt16_i64 : Int64 = 0xFFFF
UInt16(maxUInt16_i64)
var small_u16 : Int16 = 0xF
UInt8(small_u16)

// Signed to signed truncation.
var minInt8_i64 : Int64 = -128
Int8(minInt8_i64)
Int16(minInt8_i64)
var zero_i64 : Int64 = 0
Int8(zero_i64)
