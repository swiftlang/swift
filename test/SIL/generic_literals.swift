// RUN: %swift -emit-sil %s | FileCheck %s

// CHECK: sil @_T16generic_literals21genericIntegerLiteralUSs25IntegerLiteralConvertible_Ss32BuiltinIntegerLiteralConvertible__FT1xQ__T_
func genericIntegerLiteral<T : IntegerLiteralConvertible>(x : T) {
  // CHECK: [[TMETA:%.*]] = metatype $T.metatype
  // CHECK: [[TCONV:%.*]] = archetype_method $$*T, @convertFromIntegerLiteral.1
  // CHECK: [[LITMETA:%.*]] = metatype $T.IntegerLiteralType.metatype
  // CHECK: [[BUILTINCONV:%.*]] = archetype_method $$*T.IntegerLiteralType, @_convertFromBuiltinIntegerLiteral.1
  // CHECK: [[INTLIT:%.*]] = integer_literal $Builtin.Int128, 17
  // CHECK: [[LITVAR:%.*]] = alloc_var stack $T.IntegerLiteralType
  // CHECK: [[LIT:%.*]] = apply [[BUILTINCONV]]([[LITVAR]], [[INTLIT]], [[LITMETA]]) : $((value : Builtin.Int128), T.IntegerLiteralType.metatype) -> T.IntegerLiteralType
  // CHECK: [[ADDR:%.*]] = alloc_var stack $T
  // CHECK: apply [[TCONV]]([[ADDR]], [[LITVAR]], [[TMETA]]) : $((value : T.IntegerLiteralType), T.metatype) -> T

  x = 17
}

// CHECK: sil @_T16generic_literals22genericFloatingLiteralUSs23FloatLiteralConvertible_Ss30BuiltinFloatLiteralConvertible__FT1xQ__T_ : $[thin] <T : FloatLiteralConvertible> (x : T) -> () 
func genericFloatingLiteral<T : FloatLiteralConvertible>(x : T) {
  // CHECK: [[TMETA:%.*]] = metatype $T.metatype
  // CHECK: [[CONV:%.*]] = archetype_method $$*T, @convertFromFloatLiteral.1
  // CHECK: [[TFLT_META:%.*]] = metatype $T.FloatLiteralType.metatype
  // CHECK: [[BUILTIN_CONV:%.*]] = archetype_method $$*T.FloatLiteralType, @_convertFromBuiltinFloatLiteral.1
  // CHECK: [[LIT_VALUE:%.*]] = float_literal $Builtin.FP_IEEE64, 2.5
  // CHECK: [[FLT_VAL:%.*]] = alloc_var stack $T.FloatLiteralType
  // CHECK: apply [[BUILTIN_CONV]]([[FLT_VAL]], [[LIT_VALUE]], [[TFLT_META]]) : $((value : Builtin.FP_IEEE64), T.FloatLiteralType.metatype) -> T.FloatLiteralType
  // CHECK: [[TVAL:%.*]] = alloc_var stack $T
  // CHECK: apply [[CONV]]([[TVAL]], [[FLT_VAL]], [[TMETA]]) : $((value : T.FloatLiteralType), T.metatype) -> T

  x = 2.5
}
