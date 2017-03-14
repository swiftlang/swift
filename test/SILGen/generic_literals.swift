// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil hidden @_T016generic_literals0A14IntegerLiteralyx1x_ts013ExpressibleBycD0RzlF
func genericIntegerLiteral<T : ExpressibleByIntegerLiteral>(x: T) {
  var x = x
  // CHECK: [[TCONV:%.*]] = witness_method $T, #ExpressibleByIntegerLiteral.init!allocator.1
  // CHECK: [[ADDR:%.*]] = alloc_stack $T
  // CHECK: [[TMETA:%.*]] = metatype $@thick T.Type
  // CHECK: [[BUILTINCONV:%.*]] = witness_method $T.IntegerLiteralType, #_ExpressibleByBuiltinIntegerLiteral.init!allocator.1
  // CHECK: [[LITVAR:%.*]] = alloc_stack $T.IntegerLiteralType
  // CHECK: [[LITMETA:%.*]] = metatype $@thick T.IntegerLiteralType.Type
  // CHECK: [[INTLIT:%.*]] = integer_literal $Builtin.Int2048, 17
  // CHECK: [[LIT:%.*]] = apply [[BUILTINCONV]]<T.IntegerLiteralType>([[LITVAR]], [[INTLIT]], [[LITMETA]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : _ExpressibleByBuiltinIntegerLiteral> (Builtin.Int2048, @thick τ_0_0.Type) -> @out τ_0_0
  // CHECK: apply [[TCONV]]<T>([[ADDR]], [[LITVAR]], [[TMETA]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : ExpressibleByIntegerLiteral> (@in τ_0_0.IntegerLiteralType, @thick τ_0_0.Type) -> @out τ_0_0

  x = 17
}

// CHECK-LABEL: sil hidden @_T016generic_literals0A15FloatingLiteral{{[_0-9a-zA-Z]*}}F
func genericFloatingLiteral<T : ExpressibleByFloatLiteral>(x: T) {
  var x = x
  // CHECK: [[CONV:%.*]] = witness_method $T, #ExpressibleByFloatLiteral.init!allocator.1
  // CHECK: [[TVAL:%.*]] = alloc_stack $T
  // CHECK: [[TMETA:%.*]] = metatype $@thick T.Type
  // CHECK: [[BUILTIN_CONV:%.*]] = witness_method $T.FloatLiteralType, #_ExpressibleByBuiltinFloatLiteral.init!allocator.1
  // CHECK: [[FLT_VAL:%.*]] = alloc_stack $T.FloatLiteralType
  // CHECK: [[TFLT_META:%.*]] = metatype $@thick T.FloatLiteralType.Type
  // CHECK: [[LIT_VALUE:%.*]] = float_literal $Builtin.FPIEEE{{64|80}}, {{0x4004000000000000|0x4000A000000000000000}}
  // CHECK: apply [[BUILTIN_CONV]]<T.FloatLiteralType>([[FLT_VAL]], [[LIT_VALUE]], [[TFLT_META]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : _ExpressibleByBuiltinFloatLiteral> (Builtin.FPIEEE{{64|80}}, @thick τ_0_0.Type) -> @out τ_0_0
  // CHECK: apply [[CONV]]<T>([[TVAL]], [[FLT_VAL]], [[TMETA]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : ExpressibleByFloatLiteral> (@in τ_0_0.FloatLiteralType, @thick τ_0_0.Type) -> @out τ_0_0

  x = 2.5
}
