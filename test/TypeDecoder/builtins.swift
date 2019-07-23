// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/generics -emit-module
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/generics -type-from-mangled=%t/input | %FileCheck %s

// DEMANGLE: $sBbD
// DEMANGLE: $sBoD
// DEMANGLE: $sBpD
// DEMANGLE: $sBwD
// CHECK: BridgeObject
// CHECK: NativeObject
// CHECK: RawPointer
// CHECK: Word

// DEMANGLE: $sBID
// CHECK: IntLiteral

// DEMANGLE: $sBf32_D
// DEMANGLE: $sBf64_D
// DEMANGLE: $sBf80_D
// CHECK: FPIEEE32
// CHECK: FPIEEE64
// CHECK: FPIEEE80

// DEMANGLE: $sBi1_D
// DEMANGLE: $sBi8_D
// DEMANGLE: $sBi16_D
// DEMANGLE: $sBi32_D
// DEMANGLE: $sBi64_D
// DEMANGLE: $sBi128_D
// CHECK: Int1
// CHECK: Int8
// CHECK: Int16
// CHECK: Int32
// CHECK: Int64
// CHECK: Int128

// DEMANGLE: $sBf32_Bv2_D
// DEMANGLE: $sBf32_Bv4_D
// DEMANGLE: $sBf32_Bv8_D
// DEMANGLE: $sBf32_Bv16_D
// DEMANGLE: $sBf32_Bv32_D
// DEMANGLE: $sBf32_Bv64_D
// CHECK: Vec2xFPIEEE32
// CHECK: Vec4xFPIEEE32
// CHECK: Vec8xFPIEEE32
// CHECK: Vec16xFPIEEE32
// CHECK: Vec32xFPIEEE32
// CHECK: Vec64xFPIEEE32

// DEMANGLE: $sBf64_Bv2_D
// DEMANGLE: $sBf64_Bv4_D
// DEMANGLE: $sBf64_Bv8_D
// DEMANGLE: $sBf64_Bv16_D
// DEMANGLE: $sBf64_Bv32_D
// DEMANGLE: $sBf64_Bv64_D
// CHECK: Vec2xFPIEEE64
// CHECK: Vec4xFPIEEE64
// CHECK: Vec8xFPIEEE64
// CHECK: Vec16xFPIEEE64
// CHECK: Vec32xFPIEEE64
// CHECK: Vec64xFPIEEE64

// DEMANGLE: $sBi16_Bv2_D
// DEMANGLE: $sBi16_Bv4_D
// DEMANGLE: $sBi16_Bv8_D
// DEMANGLE: $sBi16_Bv16_D
// DEMANGLE: $sBi16_Bv32_D
// DEMANGLE: $sBi16_Bv64_D
// CHECK: Vec2xInt16
// CHECK: Vec4xInt16
// CHECK: Vec8xInt16
// CHECK: Vec16xInt16
// CHECK: Vec32xInt16
// CHECK: Vec64xInt16

// DEMANGLE: $sBi32_Bv2_D
// DEMANGLE: $sBi32_Bv4_D
// DEMANGLE: $sBi32_Bv8_D
// DEMANGLE: $sBi32_Bv16_D
// DEMANGLE: $sBi32_Bv32_D
// DEMANGLE: $sBi32_Bv64_D
// CHECK: Vec2xInt32
// CHECK: Vec4xInt32
// CHECK: Vec8xInt32
// CHECK: Vec16xInt32
// CHECK: Vec32xInt32
// CHECK: Vec64xInt32

// DEMANGLE: $sBi64_Bv2_D
// DEMANGLE: $sBi64_Bv4_D
// DEMANGLE: $sBi64_Bv8_D
// DEMANGLE: $sBi64_Bv16_D
// DEMANGLE: $sBi64_Bv32_D
// DEMANGLE: $sBi64_Bv64_D
// CHECK: Vec2xInt64
// CHECK: Vec4xInt64
// CHECK: Vec8xInt64
// CHECK: Vec16xInt64
// CHECK: Vec32xInt64
// CHECK: Vec64xInt64

// DEMANGLE: $sBi8_Bv2_D
// DEMANGLE: $sBi8_Bv4_D
// DEMANGLE: $sBi8_Bv8_D
// DEMANGLE: $sBi8_Bv16_D
// DEMANGLE: $sBi8_Bv32_D
// DEMANGLE: $sBi8_Bv64_D
// CHECK: Vec2xInt8
// CHECK: Vec4xInt8
// CHECK: Vec8xInt8
// CHECK: Vec16xInt8
// CHECK: Vec32xInt8
// CHECK: Vec64xInt8