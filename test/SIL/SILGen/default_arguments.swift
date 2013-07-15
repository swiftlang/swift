// RUN: %swift -emit-silgen %s | FileCheck %s

// Default argument for first parameter.
// CHECK: sil @_T17default_arguments7defarg1FT1iSi1dSd1sSS_T_e_ : $[thin] () -> Int64
// CHECK: [[CVT:%[0-9]+]] = function_ref @_TSi33_convertFromBuiltinIntegerLiteralfMSiFT3valBi128__Si
// CHECK: [[INT:%[0-9]+]] = metatype $Int64.metatype
// CHECK: [[LIT:%[0-9]+]] = integer_literal $Builtin.Int128, 17
// CHECK: [[RESULT:%[0-9]+]] = apply [[CVT]]([[LIT]], [[INT]]) : $[thin] ((val : Builtin.Int128), Int64.metatype) -> Int64
// CHECK: return [[RESULT]] : $Int64

// Default argument for third parameter.
// CHECK: sil @_T17default_arguments7defarg1FT1iSi1dSd1sSS_T_e1_ : $[thin] () -> String
// CHECK: [[CVT:%[0-9]+]] = function_ref @_TSS32_convertFromBuiltinStringLiteralfMSSFT5valueBp8byteSizeBi64_7isASCIIBi1__SS : $[thin]
// CHECK: [[STRING:%[0-9]+]] = metatype $String.metatype
// CHECK: [[LIT:%[0-9]+]] = string_literal $(Builtin.RawPointer, Builtin.Int64, Builtin.Int1), "Hello"
// CHECK: [[LITPTR:%[0-9]+]] = tuple_extract [[LIT]], 0
// CHECK: [[LITLEN:%[0-9]+]] = tuple_extract [[LIT]], 1
// CHECK: [[LITASCII:%[0-9]+]] = tuple_extract [[LIT]], 2
// CHECK: [[RESULT:%[0-9]+]] = apply [[CVT]]([[LITPTR]], [[LITLEN]], [[LITASCII]], [[STRING]]) : $[thin]
// CHECK: return [[RESULT]] : $String
func defarg1(i : Int = 17, d : Double, s : String = "Hello") { }

