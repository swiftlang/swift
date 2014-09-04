// RUN: %swift %s -emit-sil -Ounchecked -o - -verify | FileCheck %s

// rdar://17406327
// CHECK-LABEL: sil @_TF22peephole_trunc_and_ext29test_trunc_u_to_s_zext_lshr_1FSuSi : $@thin (UInt) -> Int
func test_trunc_u_to_s_zext_lshr_1(x:UInt) -> Word {
    // CHECK: builtin_function_ref "lshr_Word"
    // CHECK-NOT: builtin_function_ref "zextOrBitCast_Word_Int64"
    // CHECK-NOT: builtin_function_ref "u_to_s_checked_conversion_Int64"
    // CHECK-NOT: builtin_function_ref "truncOrBitCast_Int64_Word"
    // CHECK: return
    let v1: Word =  Word(Int64(x >> 1))
    return v1
}

// CHECK-LABEL: sil @_TF22peephole_trunc_and_ext30test_trunc_u_to_s_zext_lshr_10FSuSi : $@thin (UInt) -> Int
func test_trunc_u_to_s_zext_lshr_10(x:UInt) -> Word {
    // CHECK: builtin_function_ref "lshr_Word"
    // CHECK-NOT: builtin_function_ref "zextOrBitCast_Word_Int64"
    // CHECK-NOT: builtin_function_ref "u_to_s_checked_conversion_Int64"
    // CHECK-NOT: builtin_function_ref "truncOrBitCast_Int64_Word"
    // CHECK: return
    let v1: Word =  Word(Int64(x >> 10))
    return v1
}

// CHECK-LABEL: sil @_TF22peephole_trunc_and_ext31test_trunc_u_to_s_zext_lshr_varFTSuSu_Si : $@thin (UInt, UInt) -> Int
// This should not trigger the optimization as it is not
// known if c > 0
func test_trunc_u_to_s_zext_lshr_var(x:UInt, c:UInt) -> Word {
    // CHECK: builtin_function_ref "lshr_Word"
    // CHECK: builtin_function_ref "zextOrBitCast_Word_Int64"
    // CHECK: builtin_function_ref "u_to_s_checked_conversion_Int64"
    // CHECK: builtin_function_ref "truncOrBitCast_Int64_Word"
    // CHECK: return
    let v1: Word =  Word(Int64(x >> c))
    return v1
}

// CHECK-LABEL: sil @_TF22peephole_trunc_and_ext36test_uint16_trunc_u_to_s_zext_lshr_1FVSs6UInt16S0_ : $@thin (UInt16) -> UInt16
func test_uint16_trunc_u_to_s_zext_lshr_1(x:UInt16) -> UInt16 {
    // CHECK: builtin_function_ref "lshr_Int16"
    // CHECK-NOT: builtin_function_ref "zext_Int16_Int32"
    // CHECK-NOT: builtin_function_ref "s_to_u_checked_trunc_Int32_Int16"
    // CHECK-NOT: tuple_extract
    // CHECK: return
    let v1: UInt16 =  UInt16(Int32(x >> 1))
    return v1
}

// CHECK-LABEL: sil @_TF22peephole_trunc_and_ext35test_int16_trunc_u_to_s_zext_lshr_1FVSs6UInt16VSs5Int16 : $@thin (UInt16) -> Int16
func test_int16_trunc_u_to_s_zext_lshr_1(x:UInt16) -> Int16 {
    // CHECK: builtin_function_ref "lshr_Int16"
    // CHECK-NOT: builtin_function_ref "zext_Int16_Int32"
    // CHECK-NOT: builtin_function_ref "s_to_u_checked_trunc_Int32_Int16"
    // CHECK-NOT: tuple_extract
    // CHECK: return
    let v1: Int16 =  Int16(Int32(x >> 1))
    return v1
}

// CHECK-LABEL: sil @_TF22peephole_trunc_and_ext26test_trunc_s_to_u_zext_varFSiSu : $@thin (Int) -> UInt
// rdar://17433082
// This should not trigger the optimization as it is not
// known if top bit of x is set
func test_trunc_s_to_u_zext_var(x:Int) -> UInt {
    // CHECK: builtin_function_ref "zextOrBitCast_Word_Int64"
    // CHECK: builtin_function_ref "s_to_u_checked_conversion_Int64"
    // CHECK: builtin_function_ref "truncOrBitCast_Int64_Word"
    // CHECK: return
    return UInt(UInt64(x))
}

// CHECK-LABEL: sil @_TF22peephole_trunc_and_ext29test_trunc_s_to_u_zext_sizeofFT_Su : $@thin () -> UInt
// rdar://17433082
// sizeof is known to return strictly positive values
func test_trunc_s_to_u_zext_sizeof() -> UInt {
    // CHECK: builtin_function_ref "sizeof"
    // CHECK-NOT: builtin_function_ref "zextOrBitCast_Word_Int64"
    // CHECK-NOT: builtin_function_ref "s_to_u_checked_conversion_Int64"
    // CHECK-NOT: builtin_function_ref "truncOrBitCast_Int64_Word"
    // CHECK: return
    return UInt(UInt64(sizeof(Int)))
}

// CHECK-LABEL: sil @_TF22peephole_trunc_and_ext35test_int16_trunc_s_to_u_zext_sizeofFT_VSs6UInt16 : $@thin () -> UInt16
// rdar://17433082
// sizeof is known to return strictly positive values
// But Word->Int64 is not a safe conversion
func test_int16_trunc_s_to_u_zext_sizeof() -> UInt16 {
    return UInt16(UInt32(sizeof(Int)))
}

// CHECK-LABEL: sil @_TF22peephole_trunc_and_ext34test_int16_trunc_s_to_u_zext_int16FVSs6UInt16S0_ : $@thin (UInt16) -> UInt16
func test_int16_trunc_s_to_u_zext_int16(x:UInt16) -> UInt16 {
    // CHECK: builtin_function_ref "lshr_Int16"
    // CHECK-NOT: builtin_function_ref "u_to_s_checked_conversion_Int16"
    // CHECK-NOT: builtin_function_ref "s_to_u_checked_conversion_Int16"
    // CHECK-NOT: builtin_function_ref "sext_Int16_Int32"
    // CHECK-NOT: builtin_function_ref "u_to_u_checked_trunc_Int32_Int16"
    // CHECK-NOT: tuple_extract
    // CHECK: return
    return UInt16(UInt32(Int16(x>>1)))
}

