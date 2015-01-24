// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// CHECK-LABEL: sil hidden @_TF16generic_literals21genericIntegerLitera
func genericIntegerLiteral<T : IntegerLiteralConvertible>(var x: T) {
  // CHECK: [[TCONV:%.*]] = witness_method $T, #IntegerLiteralConvertible.init!allocator.1
  // CHECK: [[TMETA:%.*]] = metatype $@thick T.Type
  // CHECK: [[BUILTINCONV:%.*]] = witness_method $T.IntegerLiteralType, #_BuiltinIntegerLiteralConvertible.init!allocator.1
  // CHECK: [[LITMETA:%.*]] = metatype $@thick T.IntegerLiteralType.Type
  // CHECK: [[INTLIT:%.*]] = integer_literal $Builtin.Int2048, 17
  // CHECK: [[LITVAR:%.*]] = alloc_stack $T.IntegerLiteralType
  // CHECK: [[LIT:%.*]] = apply [[BUILTINCONV]]<T.IntegerLiteralType>([[LITVAR]]#1, [[INTLIT]], [[LITMETA]]) : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : _BuiltinIntegerLiteralConvertible> (@out τ_0_0, Builtin.Int2048, @thick τ_0_0.Type) -> ()
  // CHECK: [[ADDR:%.*]] = alloc_stack $T
  // CHECK: apply [[TCONV]]<T, T.IntegerLiteralType>([[ADDR]]#1, [[LITVAR]]#1, [[TMETA]]) : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : IntegerLiteralConvertible, τ_0_0.IntegerLiteralType : _BuiltinIntegerLiteralConvertible> (@out τ_0_0, @in τ_0_0.IntegerLiteralType, @thick τ_0_0.Type) -> ()

  x = 17
}

// CHECK-LABEL: sil hidden @_TF16generic_literals22genericFloatingLiteral
func genericFloatingLiteral<T : FloatLiteralConvertible>(var x: T) {
  // CHECK: [[CONV:%.*]] = witness_method $T, #FloatLiteralConvertible.init!allocator.1
  // CHECK: [[TMETA:%.*]] = metatype $@thick T.Type
  // CHECK: [[BUILTIN_CONV:%.*]] = witness_method $T.FloatLiteralType, #_BuiltinFloatLiteralConvertible.init!allocator.1
  // CHECK: [[TFLT_META:%.*]] = metatype $@thick T.FloatLiteralType.Type
  // CHECK: [[LIT_VALUE:%.*]] = float_literal $Builtin.FPIEEE{{64|80}}, {{0x4004000000000000|0x4000A000000000000000}}
  // CHECK: [[FLT_VAL:%.*]] = alloc_stack $T.FloatLiteralType
  // CHECK: apply [[BUILTIN_CONV]]<T.FloatLiteralType>([[FLT_VAL]]#1, [[LIT_VALUE]], [[TFLT_META]]) : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : _BuiltinFloatLiteralConvertible> (@out τ_0_0, Builtin.FPIEEE{{64|80}}, @thick τ_0_0.Type) -> ()
  // CHECK: [[TVAL:%.*]] = alloc_stack $T
  // CHECK: apply [[CONV]]<T, T.FloatLiteralType>([[TVAL]]#1, [[FLT_VAL]]#1, [[TMETA]]) : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : FloatLiteralConvertible, τ_0_0.FloatLiteralType : _BuiltinFloatLiteralConvertible> (@out τ_0_0, @in τ_0_0.FloatLiteralType, @thick τ_0_0.Type) -> ()

  x = 2.5
}
