// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name main -typecheck -verify %s

import c_simd

let char2_value: char2 = makes_char2()
let char64_value: char64 = makes_char64()
let uchar3_value: uchar3 = makes_uchar3()
let uchar32_value: uchar32 = makes_uchar32()
let short3_value: short3 = makes_short3()
let short8_value: short8 = makes_short8()
let ushort1_value: ushort1 = makes_ushort1()
let ushort16_value: ushort16 = makes_ushort16()
let int3_value: int3 = makes_int3()
let int32_value: int32 = makes_int32()
let uint4_value: uint4 = makes_uint4()
let uint2_value: uint2 = makes_uint2()
let long2_value: long2 = makes_long2()
let long8_value: long8 = makes_long8()
let ulong4_value: ulong4 = makes_ulong4()
let ulong1_value: ulong1 = makes_ulong1()
let ll3_value: ll3 = makes_ll3()
let ll8_value: ll8 = makes_ll8()
let ull4_value: ull4 = makes_ull4()
let ull16_value: ull16 = makes_ull16()
let float2_value: float2 = makes_float2()
let float3_value: float3 = makes_float3()
let float4_value: float4 = makes_float4()
let float8_value: float8 = makes_float8()
let float16_value: float16 = makes_float16()
let double2_value: double2 = makes_double2()
let double3_value: double3 = makes_double3()
let double4_value: double4 = makes_double4()
let double8_value: double8 = makes_double8()

takes_char2(char2_value)
takes_char64(char64_value)
takes_uchar3(uchar3_value)
takes_uchar32(uchar32_value)
takes_short3(short3_value)
takes_short8(short8_value)
takes_ushort1(ushort1_value)
takes_ushort16(ushort16_value)
takes_int3(int3_value)
takes_int32(int32_value)
takes_uint4(uint4_value)
takes_uint2(uint2_value)
takes_long2(long2_value)
takes_long8(long8_value)
takes_ulong4(ulong4_value)
takes_ulong1(ulong1_value)
takes_ll3(ll3_value)
takes_ll8(ll8_value)
takes_ull4(ull4_value)
takes_ull16(ull16_value)
takes_float2(float2_value)
takes_float3(float3_value)
takes_float4(float4_value)
takes_float8(float8_value)
takes_float16(float16_value)
takes_double2(double2_value)
takes_double3(double3_value)
takes_double4(double4_value)
takes_double8(double8_value)

// These shouldn't be imported, since there's no type to map them to.

let char17_value = makes_char17()   // expected-error{{unresolved identifier 'makes_char17'}}
let uchar21_value = makes_uchar21() // expected-error{{unresolved identifier 'makes_uchar21'}}
let short5_value = makes_short5()   // expected-error{{unresolved identifier 'makes_short5'}}
let ushort6_value = makes_ushort6() // expected-error{{unresolved identifier 'makes_ushort6'}}
let int128_value = makes_int128()   // expected-error{{unresolved identifier 'makes_int128'}}
let uint20_value = makes_uint20()   // expected-error{{unresolved identifier 'makes_uint20'}}

takes_char17(char17_value)   // expected-error{{unresolved identifier 'takes_char17'}}
takes_uchar21(uchar21_value) // expected-error{{unresolved identifier 'takes_uchar21'}}
takes_short5(short5_value)   // expected-error{{unresolved identifier 'takes_short5'}}
takes_ushort6(ushort6_value) // expected-error{{unresolved identifier 'takes_ushort6'}}
takes_int128(int128_value)   // expected-error{{unresolved identifier 'takes_int128'}}
takes_uint20(uint20_value)   // expected-error{{unresolved identifier 'takes_uint20'}}
