// RUN: %swift -emit-silgen %s | FileCheck %s

// CHECK: sil @_T16generic_literals21genericIntegerLiteralUSs25IntegerLiteralConvertible_Ss32BuiltinIntegerLiteralConvertible__FT1xQ__T_
func genericIntegerLiteral<T : IntegerLiteralConvertible>(x : T) {
  // CHECK: [[TMETA:%.*]] = metatype $T.metatype
  // CHECK: [[TCONV:%.*]] = archetype_method $*T, #IntegerLiteralConvertible.convertFromIntegerLiteral!1
  // CHECK: [[LITMETA:%.*]] = metatype $T.IntegerLiteralType.metatype
  // CHECK: [[BUILTINCONV:%.*]] = archetype_method $*T.IntegerLiteralType, #BuiltinIntegerLiteralConvertible._convertFromBuiltinIntegerLiteral!1
  // CHECK: [[INTLIT:%.*]] = integer_literal $Builtin.Int128, 17
  // CHECK: [[LITVAR:%.*]] = alloc_stack $T.IntegerLiteralType
  // CHECK: [[LIT:%.*]] = apply [[BUILTINCONV]]([[LITVAR]]#1, [[INTLIT]], [[LITMETA]]) : $((value : Builtin.Int128), T.IntegerLiteralType.metatype) -> T.IntegerLiteralType
  // CHECK: [[ADDR:%.*]] = alloc_stack $T
  // CHECK: apply [[TCONV]]([[ADDR]]#1, [[LITVAR]]#1, [[TMETA]]) : $((value : T.IntegerLiteralType), T.metatype) -> T

  x = 17
}

// CHECK: sil @_T16generic_literals22genericFloatingLiteralUSs23FloatLiteralConvertible_Ss30BuiltinFloatLiteralConvertible__FT1xQ__T_ : $[thin] <T : FloatLiteralConvertible> (x : T) -> () 
func genericFloatingLiteral<T : FloatLiteralConvertible>(x : T) {
  // CHECK: [[TMETA:%.*]] = metatype $T.metatype
  // CHECK: [[CONV:%.*]] = archetype_method $*T, #FloatLiteralConvertible.convertFromFloatLiteral!1
  // CHECK: [[TFLT_META:%.*]] = metatype $T.FloatLiteralType.metatype
  // CHECK: [[BUILTIN_CONV:%.*]] = archetype_method $*T.FloatLiteralType, #BuiltinFloatLiteralConvertible._convertFromBuiltinFloatLiteral!1
  // CHECK: [[LIT_VALUE:%.*]] = float_literal $Builtin.FPIEEE64, 0x4004000000000000
  // CHECK: [[FLT_VAL:%.*]] = alloc_stack $T.FloatLiteralType
  // CHECK: apply [[BUILTIN_CONV]]([[FLT_VAL]]#1, [[LIT_VALUE]], [[TFLT_META]]) : $((value : Builtin.FPIEEE64), T.FloatLiteralType.metatype) -> T.FloatLiteralType
  // CHECK: [[TVAL:%.*]] = alloc_stack $T
  // CHECK: apply [[CONV]]([[TVAL]]#1, [[FLT_VAL]]#1, [[TMETA]]) : $((value : T.FloatLiteralType), T.metatype) -> T

  x = 2.5
}
