// RUN: %target-swift-emit-silgen -enable-experimental-feature VariadicGenerics %s | %FileCheck %s
// REQUIRES: asserts

func copyOrThrow<T>(value: T) throws -> T { return value }

// CHECK-LABEL: @$s4main13copyIntoTupleyxxQp_txxQpRvzlF : $@convention(thin) <each T> (@pack_guaranteed Pack{repeat each T}) -> @pack_out Pack{repeat each T} {
// CHECK:       bb0([[OUT:%0]] : $*Pack{repeat each T}, [[IN:%1]] : $*Pack{repeat each T}):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LEN:%.*]] = pack_length $Pack{repeat each T}
// CHECK-NEXT:    br bb1([[ZERO]] : $Builtin.Word)
// CHECK:       bb1([[IDX:%.*]] : $Builtin.Word)
// CHECK-NEXT:    [[IDX_EQ_LEN:%.*]] = builtin "cmp_eq_Word"([[IDX]] : $Builtin.Word, [[LEN]] : $Builtin.Word) : $Builtin.Int1
// CHECK-NEXT:     cond_br [[IDX_EQ_LEN]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[INDEX:%.*]] = dynamic_pack_index [[IDX]] of $Pack{repeat each T}
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[DEST_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of [[OUT]] : $*Pack{repeat each T} as $*@pack_element([[UUID]]) T
// CHECK-NEXT:    [[SRC_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of [[IN]] : $*Pack{repeat each T} as $*@pack_element([[UUID]]) T
// CHECK-NEXT:    copy_addr [[SRC_ELT_ADDR]] to [init] [[DEST_ELT_ADDR]] : $*@pack_element([[UUID]]) T
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb1([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb3:
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
func copyIntoTuple<each T>(_ args: repeat each T) -> (repeat each T) {
  return (repeat each args)
}

// CHECK-LABEL: @$s4main20copyOrThrowIntoTupleyxxQp_txxQpKRvzlF : $@convention(thin) <each T> (@pack_guaranteed Pack{repeat each T}) -> (@pack_out Pack{repeat each T}, @error any Error) {
// CHECK:       bb0([[OUT:%0]] : $*Pack{repeat each T}, [[IN:%1]] : $*Pack{repeat each T}):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LEN:%.*]] = pack_length $Pack{repeat each T}
// CHECK-NEXT:    br bb1([[ZERO]] : $Builtin.Word)
// CHECK:       bb1([[IDX:%.*]] : $Builtin.Word)
// CHECK-NEXT:    [[IDX_EQ_LEN:%.*]] = builtin "cmp_eq_Word"([[IDX]] : $Builtin.Word, [[LEN]] : $Builtin.Word) : $Builtin.Int1
// CHECK-NEXT:     cond_br [[IDX_EQ_LEN]], bb4, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[INDEX:%.*]] = dynamic_pack_index [[IDX]] of $Pack{repeat each T}
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[DEST_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of [[OUT]] : $*Pack{repeat each T} as $*@pack_element([[UUID]]) T
// CHECK-NEXT:    [[SRC_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of [[IN]] : $*Pack{repeat each T} as $*@pack_element([[UUID]]) T
//   FIXME: make this a borrow
// CHECK-NEXT:    [[TEMP:%.*]] = alloc_stack $@pack_element([[UUID]]) T
// CHECK-NEXT:    copy_addr [[SRC_ELT_ADDR]] to [init] [[TEMP]] : $*@pack_element([[UUID]]) T
// CHECK-NEXT:    // function_ref copyOrThrow
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4main11copyOrThrow5valuexx_tKlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> (@out τ_0_0, @error any Error)
// CHECK-NEXT:    try_apply [[FN]]<@pack_element([[UUID]]) T>([[DEST_ELT_ADDR]], [[TEMP]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> (@out τ_0_0, @error any Error), normal bb3, error bb5
// CHECK:       bb3{{.*}}:
// CHECK-NEXT:    destroy_addr [[TEMP]] : $*@pack_element([[UUID]]) T
// CHECK-NEXT:    dealloc_stack [[TEMP]] : $*@pack_element([[UUID]]) T
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb1([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb4:
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
// CHECK:       bb5([[ERROR:%.*]] : @owned $any Error):
// CHECK-NEXT:    destroy_addr [[TEMP]] : $*@pack_element([[UUID]]) T
// CHECK-NEXT:    dealloc_stack [[TEMP]] : $*@pack_element([[UUID]]) T
//   Error-path loop to destroy the partial pack expansion prior to [[IDX]]
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    br bb6([[IDX]] : $Builtin.Word)
// CHECK:       bb6([[LAST_IDX:%.*]] : $Builtin.Word)
// CHECK-NEXT:    [[LAST_IDX_EQ_ZERO:%.*]] = builtin "cmp_eq_Word"([[LAST_IDX]] : $Builtin.Word, [[ZERO]] : $Builtin.Word) : $Builtin.Int1
// CHECK-NEXT:     cond_br [[LAST_IDX_EQ_ZERO]], bb8, bb7
// CHECK:       bb7:
// CHECK-NEXT:    [[DESTROY_IDX:%.*]] = builtin "sub_Word"([[LAST_IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    [[DESTROY_INDEX:%.*]] = dynamic_pack_index [[DESTROY_IDX]] of $Pack{repeat each T}
// CHECK-NEXT:    open_pack_element [[DESTROY_INDEX]] of <each T> at <Pack{repeat each T}>, shape $T, uuid [[DESTROY_UUID:".*"]]
// CHECK-NEXT:    [[DESTROY_ELT_ADDR:%.*]] = pack_element_get [[DESTROY_INDEX]] of [[OUT]] : $*Pack{repeat each T} as $*@pack_element([[DESTROY_UUID]]) T
// CHECK-NEXT:    destroy_addr [[DESTROY_ELT_ADDR]] : $*@pack_element([[DESTROY_UUID]]) T
// CHECK-NEXT:    br bb6([[DESTROY_IDX]] : $Builtin.Word)
// CHECK:       bb8:
// CHECK-NEXT:    throw [[ERROR]] : $any Error
func copyOrThrowIntoTuple<each T>(_ args: repeat each T) throws -> (repeat each T) {
  return (repeat try copyOrThrow(value: each args))
}

