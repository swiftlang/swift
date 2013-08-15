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
// CHECK: [[LITPTR:%[0-9]+]] = tuple_extract [[LIT]] : {{.*}}, 0
// CHECK: [[LITLEN:%[0-9]+]] = tuple_extract [[LIT]] : {{.*}}, 1
// CHECK: [[LITASCII:%[0-9]+]] = tuple_extract [[LIT]] : {{.*}}, 2
// CHECK: [[RESULT:%[0-9]+]] = apply [[CVT]]([[LITPTR]], [[LITLEN]], [[LITASCII]], [[STRING]]) : $[thin]
// CHECK: return [[RESULT]] : $String
func defarg1(i : Int = 17, d : Double, s : String = "Hello") { }

// CHECK: sil @_T17default_arguments15testDefaultArg1FT_T_
func testDefaultArg1() {
  // CHECK: [[FNREF:%[0-9]+]] = function_ref @_T17default_arguments7defarg1FT1iSi1dSd1sSS_T_
  // CHECK: [[LITFN:%[0-9]+]] = function_ref @_TSd31_convertFromBuiltinFloatLiteralfMSdFT5valueBf64__Sd
  // CHECK: [[FLOAT64:%[0-9]+]] = metatype $Float64.metatype
  // CHECK: [[FLOATLIT:%[0-9]+]] = float_literal $Builtin.FPIEEE64, 0x4009000000000000
  // CHECK: [[FLOATVAL:%[0-9]+]] = apply [[LITFN]]([[FLOATLIT]], [[FLOAT64]])
  // CHECK: [[DEF0FN:%[0-9]+]] = function_ref @_T17default_arguments7defarg1FT1iSi1dSd1sSS_T_e_
  // CHECK: [[DEF0:%[0-9]+]] = apply [[DEF0FN]]()
  // CHECK: [[DEF2FN:%[0-9]+]] = function_ref @_T17default_arguments7defarg1FT1iSi1dSd1sSS_T_e1_
  // CHECK: [[DEF2:%[0-9]+]] = apply [[DEF2FN]]()
  // CHECK: apply [[FNREF]]([[DEF0]], [[FLOATVAL]], [[DEF2]])
  defarg1(d:3.125)
}

func defarg2(i : Int, d : Double = 3.125, s : String = "Hello") { }

// CHECK: sil @_T17default_arguments15testDefaultArg2FT_T_
func testDefaultArg2() {
// CHECK:  [[FNREF:%[0-9]+]] = function_ref @_T17default_arguments7defarg2FT1iSi1dSd1sSS_T_ : $[thin] (i : Int64, d : Float64, s : String) -> ()
// CHECK:  [[LITFN:%[0-9]+]] = function_ref @_TSi33_convertFromBuiltinIntegerLiteralfMSiFT3valBi128__Si : $[thin] ((val : Builtin.Int128), Int64.metatype) -> Int64
// CHECK:  [[INT64:%[0-9]+]] = metatype $Int64.metatype
// CHECK:  [[INTLIT:%[0-9]+]] = integer_literal $Builtin.Int128, 5
// CHECK:  [[I:%[0-9]+]] = apply [[LITFN]]([[INTLIT]], [[INT64]]) : $[thin] ((val : Builtin.Int128), Int64.metatype) -> Int64
// CHECK:  [[DFN:%[0-9]+]] = function_ref @_T17default_arguments7defarg2FT1iSi1dSd1sSS_T_e0_ : $[thin] () -> Float64
// CHECK:  [[D:%[0-9]+]] = apply [[DFN]]() : $[thin] () -> Float64
// CHECK:  [[SFN:%[0-9]+]] = function_ref @_T17default_arguments7defarg2FT1iSi1dSd1sSS_T_e1_ : $[thin] () -> String
// CHECK:  [[S:%[0-9]+]] = apply [[SFN]]() : $[thin] () -> String
// CHECK:  apply [[FNREF]]([[I]], [[D]], [[S]]) : $[thin] (i : Int64, d : Float64, s : String) -> ()
  defarg2(5)
}
