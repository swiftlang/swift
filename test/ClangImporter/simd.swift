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
let half2_value: half2 = makes_half2()
let half3_value: half3 = makes_half3()
let half4_value: half4 = makes_half4()
let half8_value: half8 = makes_half8()
let half16_value: half16 = makes_half16()
let half32_value: half32 = makes_half32()
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
takes_half2(half2_value)
takes_half3(half3_value)
takes_half4(half4_value)
takes_half8(half8_value)
takes_half16(half16_value)
takes_half32(half32_value)
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

let char17_value = makes_char17()   // expected-error{{cannot find 'makes_char17' in scope}}
let uchar21_value = makes_uchar21() // expected-error{{cannot find 'makes_uchar21' in scope}}
let short5_value = makes_short5()   // expected-error{{cannot find 'makes_short5' in scope}}
let ushort6_value = makes_ushort6() // expected-error{{cannot find 'makes_ushort6' in scope}}
let int128_value = makes_int128()   // expected-error{{cannot find 'makes_int128' in scope}}
let uint20_value = makes_uint20()   // expected-error{{cannot find 'makes_uint20' in scope}}

takes_char17(char17_value)   // expected-error{{cannot find 'takes_char17' in scope}}
takes_uchar21(uchar21_value) // expected-error{{cannot find 'takes_uchar21' in scope}}
takes_short5(short5_value)   // expected-error{{cannot find 'takes_short5' in scope}}
takes_ushort6(ushort6_value) // expected-error{{cannot find 'takes_ushort6' in scope}}
takes_int128(int128_value)   // expected-error{{cannot find 'takes_int128' in scope}}
takes_uint20(uint20_value)   // expected-error{{cannot find 'takes_uint20' in scope}}
