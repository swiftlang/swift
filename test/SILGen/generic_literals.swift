// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

// CHECK-LABEL: sil hidden @$S16generic_literals0A14IntegerLiteral1xyx_ts013ExpressibleBycD0RzlF : $@convention(thin) <T where T : ExpressibleByIntegerLiteral> (@in_guaranteed T) -> () {
func genericIntegerLiteral<T : ExpressibleByIntegerLiteral>(x: T) {
  var x = x
  // CHECK: [[ADDR:%.*]] = alloc_stack $T
  // CHECK: [[TMETA:%.*]] = metatype $@thick T.Type
  // CHECK: [[LITVAR:%.*]] = alloc_stack $T.IntegerLiteralType
  // CHECK: [[LITMETA:%.*]] = metatype $@thick T.IntegerLiteralType.Type
  // CHECK: [[INTLIT:%.*]] = integer_literal $Builtin.Int2048, 17
  // CHECK: [[BUILTINCONV:%.*]] = witness_method $T.IntegerLiteralType, #_ExpressibleByBuiltinIntegerLiteral.init!allocator.1
  // CHECK: [[LIT:%.*]] = apply [[BUILTINCONV]]<T.IntegerLiteralType>([[LITVAR]], [[INTLIT]], [[LITMETA]]) : $@convention(witness_method: _ExpressibleByBuiltinIntegerLiteral) <τ_0_0 where τ_0_0 : _ExpressibleByBuiltinIntegerLiteral> (Builtin.Int2048, @thick τ_0_0.Type) -> @out τ_0_0
  // CHECK: [[TCONV:%.*]] = witness_method $T, #ExpressibleByIntegerLiteral.init!allocator.1
  // CHECK: apply [[TCONV]]<T>([[ADDR]], [[LITVAR]], [[TMETA]]) : $@convention(witness_method: ExpressibleByIntegerLiteral) <τ_0_0 where τ_0_0 : ExpressibleByIntegerLiteral> (@in τ_0_0.IntegerLiteralType, @thick τ_0_0.Type) -> @out τ_0_0

  x = 17
}

// CHECK-LABEL: sil hidden @$S16generic_literals0A15FloatingLiteral1xyx_ts018ExpressibleByFloatD0RzlF : $@convention(thin) <T where T : ExpressibleByFloatLiteral> (@in_guaranteed T) -> () {
func genericFloatingLiteral<T : ExpressibleByFloatLiteral>(x: T) {
  var x = x
  // CHECK: [[TVAL:%.*]] = alloc_stack $T
  // CHECK: [[TMETA:%.*]] = metatype $@thick T.Type
  // CHECK: [[FLT_VAL:%.*]] = alloc_stack $T.FloatLiteralType
  // CHECK: [[TFLT_META:%.*]] = metatype $@thick T.FloatLiteralType.Type
  // CHECK: [[LIT_VALUE:%.*]] = float_literal $Builtin.FPIEEE{{64|80}}, {{0x4004000000000000|0x4000A000000000000000}}
  // CHECK: [[BUILTIN_CONV:%.*]] = witness_method $T.FloatLiteralType, #_ExpressibleByBuiltinFloatLiteral.init!allocator.1
  // CHECK: apply [[BUILTIN_CONV]]<T.FloatLiteralType>([[FLT_VAL]], [[LIT_VALUE]], [[TFLT_META]]) : $@convention(witness_method: _ExpressibleByBuiltinFloatLiteral) <τ_0_0 where τ_0_0 : _ExpressibleByBuiltinFloatLiteral> (Builtin.FPIEEE{{64|80}}, @thick τ_0_0.Type) -> @out τ_0_0
  // CHECK: [[CONV:%.*]] = witness_method $T, #ExpressibleByFloatLiteral.init!allocator.1
  // CHECK: apply [[CONV]]<T>([[TVAL]], [[FLT_VAL]], [[TMETA]]) : $@convention(witness_method: ExpressibleByFloatLiteral) <τ_0_0 where τ_0_0 : ExpressibleByFloatLiteral> (@in τ_0_0.FloatLiteralType, @thick τ_0_0.Type) -> @out τ_0_0

  x = 2.5
}

// CHECK-LABEL: sil hidden @$S16generic_literals0A13StringLiteral1xyx_ts013ExpressibleBycD0RzlF : $@convention(thin) <T where T : ExpressibleByStringLiteral> (@in_guaranteed T) -> () {

func genericStringLiteral<T : ExpressibleByStringLiteral>(x: T) {
  var x = x
  // CHECK: [[LIT_VALUE:%.*]] = string_literal utf8 "hello"
  // CHECK: [[TSTR_META:%.*]] = metatype $@thick T.StringLiteralType.Type
  // CHECK: [[STR_VAL:%.*]] = alloc_stack $T.StringLiteralType
  // CHECK: [[BUILTIN_CONV:%.*]] = witness_method $T.StringLiteralType, #_ExpressibleByBuiltinStringLiteral.init!allocator.1
  // CHECK: apply [[BUILTIN_CONV]]<T.StringLiteralType>([[STR_VAL]], [[LIT_VALUE]], {{.*}}, [[TSTR_META]]) : $@convention(witness_method: _ExpressibleByBuiltinStringLiteral) <τ_0_0 where τ_0_0 : _ExpressibleByBuiltinStringLiteral> (Builtin.RawPointer, Builtin.Word, Builtin.Int1, @thick τ_0_0.Type) -> @out τ_0_0
  // CHECK: [[TMETA:%.*]] = metatype $@thick T.Type
  // CHECK: [[TVAL:%.*]] = alloc_stack $T
  // CHECK: [[CONV:%.*]] = witness_method $T, #ExpressibleByStringLiteral.init!allocator.1
  // CHECK: apply [[CONV]]<T>([[TVAL]], [[STR_VAL]], [[TMETA]]) : $@convention(witness_method: ExpressibleByStringLiteral) <τ_0_0 where τ_0_0 : ExpressibleByStringLiteral> (@in τ_0_0.StringLiteralType, @thick τ_0_0.Type) -> @out τ_0_0

  x = "hello"
}
