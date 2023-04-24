// RUN: %target-swift-emit-silgen %s | %FileCheck %s

func sequence() {}

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
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[DEST_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of [[OUT]] : $*Pack{repeat each T} as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    [[SRC_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of [[IN]] : $*Pack{repeat each T} as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    copy_addr [[SRC_ELT_ADDR]] to [init] [[DEST_ELT_ADDR]] : $*@pack_element([[UUID]]) each T
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
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[DEST_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of [[OUT]] : $*Pack{repeat each T} as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    [[SRC_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of [[IN]] : $*Pack{repeat each T} as $*@pack_element([[UUID]]) each T
//   FIXME: make this a borrow
// CHECK-NEXT:    [[TEMP:%.*]] = alloc_stack $@pack_element([[UUID]]) each T
// CHECK-NEXT:    copy_addr [[SRC_ELT_ADDR]] to [init] [[TEMP]] : $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    // function_ref copyOrThrow
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4main11copyOrThrow5valuexx_tKlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> (@out τ_0_0, @error any Error)
// CHECK-NEXT:    try_apply [[FN]]<@pack_element([[UUID]]) each T>([[DEST_ELT_ADDR]], [[TEMP]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> (@out τ_0_0, @error any Error), normal bb3, error bb5
// CHECK:       bb3{{.*}}:
// CHECK-NEXT:    destroy_addr [[TEMP]] : $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    dealloc_stack [[TEMP]] : $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb1([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb4:
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
// CHECK:       bb5([[ERROR:%.*]] : @owned $any Error):
// CHECK-NEXT:    destroy_addr [[TEMP]] : $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    dealloc_stack [[TEMP]] : $*@pack_element([[UUID]]) each T
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
// CHECK-NEXT:    open_pack_element [[DESTROY_INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid [[DESTROY_UUID:".*"]]
// CHECK-NEXT:    [[DESTROY_ELT_ADDR:%.*]] = pack_element_get [[DESTROY_INDEX]] of [[OUT]] : $*Pack{repeat each T} as $*@pack_element([[DESTROY_UUID]]) each T
// CHECK-NEXT:    destroy_addr [[DESTROY_ELT_ADDR]] : $*@pack_element([[DESTROY_UUID]]) each T
// CHECK-NEXT:    br bb6([[DESTROY_IDX]] : $Builtin.Word)
// CHECK:       bb8:
// CHECK-NEXT:    throw [[ERROR]] : $any Error
func copyOrThrowIntoTuple<each T>(_ args: repeat each T) throws -> (repeat each T) {
  return (repeat try copyOrThrow(value: each args))
}

// CHECK-LABEL: @$s4main22callCopyAndDestructure1a1b1cySi_S2StF
//   Set up the result pack.
// CHECK:         [[RESULT_PACK:%.*]] = alloc_pack $Pack{Int, String, String}
// CHECK-NEXT:    [[R0:%.*]] = alloc_stack $Int
// CHECK-NEXT:    [[R0_IDX:%.*]] = scalar_pack_index 0 of $Pack{Int, String, String}
// CHECK-NEXT:    pack_element_set [[R0]] : $*Int into [[R0_IDX]] of [[RESULT_PACK]] : $*Pack{Int, String, String}
// CHECK-NEXT:    [[R1:%.*]] = alloc_stack $String
// CHECK-NEXT:    [[R1_IDX:%.*]] = scalar_pack_index 1 of $Pack{Int, String, String}
// CHECK-NEXT:    pack_element_set [[R1]] : $*String into [[R1_IDX]] of [[RESULT_PACK]] : $*Pack{Int, String, String}
// CHECK-NEXT:    [[R2:%.*]] = alloc_stack $String
// CHECK-NEXT:    [[R2_IDX:%.*]] = scalar_pack_index 2 of $Pack{Int, String, String}
// CHECK-NEXT:    pack_element_set [[R2]] : $*String into [[R2_IDX]] of [[RESULT_PACK]] : $*Pack{Int, String, String}
//   Set up the argument pack.
// CHECK:         [[ARG_PACK:%.*]] = alloc_pack $Pack{Int, String, String}
// CHECK-NEXT:    [[ARG0:%.*]] = alloc_stack $Int
// CHECK-NEXT:    store %0 to [trivial] [[ARG0]] : $*Int
// CHECK-NEXT:    [[ARG0_IDX:%.*]] = scalar_pack_index 0 of $Pack{Int, String, String}
// CHECK-NEXT:    pack_element_set [[ARG0]] : $*Int into [[ARG0_IDX]] of [[ARG_PACK]] : $*Pack{Int, String, String}
// CHECK-NEXT:    [[ARG1:%.*]] = alloc_stack $String
// CHECK-NEXT:    [[COPY1:%.*]] = copy_value %1 : $String
// CHECK-NEXT:    store [[COPY1]] to [init] [[ARG1]] : $*String
// CHECK-NEXT:    [[ARG1_IDX:%.*]] = scalar_pack_index 1 of $Pack{Int, String, String}
// CHECK-NEXT:    pack_element_set [[ARG1]] : $*String into [[ARG1_IDX]] of [[ARG_PACK]] : $*Pack{Int, String, String}
// CHECK-NEXT:    [[ARG2:%.*]] = alloc_stack $String
// CHECK-NEXT:    [[COPY2:%.*]] = copy_value %2 : $String
// CHECK-NEXT:    store [[COPY2]] to [init] [[ARG2]] : $*String
// CHECK-NEXT:    [[ARG2_IDX:%.*]] = scalar_pack_index 2 of $Pack{Int, String, String}
// CHECK-NEXT:    pack_element_set [[ARG2]] : $*String into [[ARG2_IDX]] of [[ARG_PACK]] : $*Pack{Int, String, String}
//   Perform the call.
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4main13copyIntoTupleyxxQp_txxQpRvzlF
// CHECK-NEXT:    apply [[FN]]<Pack{Int, String, String}>([[RESULT_PACK]], [[ARG_PACK]])
//   Destroy the argument pack.
// CHECK-NEXT:    destroy_addr [[ARG2]] : $*String
// CHECK-NEXT:    dealloc_stack [[ARG2]] : $*String
// CHECK-NEXT:    destroy_addr [[ARG1]] : $*String
// CHECK-NEXT:    dealloc_stack [[ARG1]] : $*String
// CHECK-NEXT:    dealloc_stack [[ARG0]] : $*Int
// CHECK-NEXT:    dealloc_pack [[ARG_PACK]] : $*Pack{Int, String, String}
//   Load from the pack and bind the locals.
// CHECK-NEXT:    [[A:%.*]] = load [trivial] [[R0]] : $*Int
// CHECK-NEXT:    debug_value [[A]] : $Int
// CHECK-NEXT:    [[B:%.*]] = load [take] [[R1]] : $*String
// CHECK-NEXT:    [[C:%.*]] = load [take] [[R2]] : $*String
// CHECK-NEXT:    debug_value [[C]] : $String
// CHECK-NEXT:    destroy_value [[B]] : $String
//   End of statement.
// CHECK-NEXT:    dealloc_stack [[R2]] : $*String
// CHECK-NEXT:    dealloc_stack [[R1]] : $*String
// CHECK-NEXT:    dealloc_stack [[R0]] : $*Int
// CHECK-NEXT:    dealloc_pack [[RESULT_PACK]] : $*Pack{Int, String, String}
//   Sequence marker.
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[SEQUENCE_FN:%.*]] = function_ref @$s4main8sequenceyyF
// CHECK-NEXT:    apply [[SEQUENCE_FN]]()
//   Leave the function.
// CHECK-NEXT:    destroy_value [[C]] : $String
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
func callCopyAndDestructure(a: Int, b: String, c: String) {
  let (a,_,c) = copyIntoTuple(a, b, c)
  sequence()
}

// CHECK-LABEL: @$s4main15callCopyAndBind4argsyxxQp_tRvzlF
//   Set up the result pack to initialize the elements of the tuple
//   we're going to bind the local variable to.
// CHECK:         [[TUPLE:%.*]] = alloc_stack [lexical] $(repeat each T), let, name "result"
// CHECK-NEXT:    [[RESULT_PACK:%.*]] = alloc_pack $Pack{repeat each T}
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LEN:%.*]] = pack_length $Pack{repeat each T}
// CHECK-NEXT:    br bb1([[ZERO]] : $Builtin.Word)
// CHECK:       bb1([[IDX:%.*]] : $Builtin.Word)
// CHECK-NEXT:    [[IDX_EQ_LEN:%.*]] = builtin "cmp_eq_Word"([[IDX]] : $Builtin.Word, [[LEN]] : $Builtin.Word) : $Builtin.Int1
// CHECK-NEXT:     cond_br [[IDX_EQ_LEN]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[INDEX:%.*]] = dynamic_pack_index [[IDX]] of $Pack{repeat each T}
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[ELT_ADDR:%.*]] = tuple_pack_element_addr [[INDEX]] of [[TUPLE]] : $*(repeat each T) as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    pack_element_set [[ELT_ADDR]] : $*@pack_element([[UUID]]) each T into [[INDEX]] of [[RESULT_PACK]] : $*Pack{repeat each T}
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb1([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb3:
//   Set up the argument pack with copies of the parameter pack.
// CHECK-NEXT:    [[ARG_PACK:%.*]] = alloc_pack $Pack{repeat each T}
// CHECK-NEXT:    [[ARG_TUPLE:%.*]] = alloc_stack $(repeat each T)
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LEN:%.*]] = pack_length $Pack{repeat each T}
// CHECK-NEXT:    br bb4([[ZERO]] : $Builtin.Word)
// CHECK:       bb4([[IDX:%.*]] : $Builtin.Word)
// CHECK-NEXT:    [[IDX_EQ_LEN:%.*]] = builtin "cmp_eq_Word"([[IDX]] : $Builtin.Word, [[LEN]] : $Builtin.Word) : $Builtin.Int1
// CHECK-NEXT:     cond_br [[IDX_EQ_LEN]], bb6, bb5
// CHECK:       bb5:
// CHECK-NEXT:    [[INDEX:%.*]] = dynamic_pack_index [[IDX]] of $Pack{repeat each T}
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[DEST_ADDR:%.*]] = tuple_pack_element_addr [[INDEX]] of [[ARG_TUPLE]] : $*(repeat each T) as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    [[SRC_ADDR:%.*]] = pack_element_get [[INDEX]] of %0 : $*Pack{repeat each T} as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    copy_addr [[SRC_ADDR]] to [init] [[DEST_ADDR]] : $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    pack_element_set [[DEST_ADDR]] : $*@pack_element([[UUID]]) each T into [[INDEX]] of [[ARG_PACK]] : $*Pack{repeat each T}
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb4([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb6:
//   Perform the call.
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4main13copyIntoTupleyxxQp_txxQpRvzlF
// CHECK-NEXT:    apply [[FN]]<Pack{repeat each T}>([[RESULT_PACK]], [[ARG_PACK]])
//   End of statement.
// CHECK-NEXT:    destroy_addr [[ARG_TUPLE]] : $*(repeat each T)
// CHECK-NEXT:    dealloc_stack [[ARG_TUPLE]] : $*(repeat each T)
// CHECK-NEXT:    dealloc_pack [[ARG_PACK]] : $*Pack{repeat each T}
// CHECK-NEXT:    dealloc_pack [[RESULT_PACK]] : $*Pack{repeat each T}
//   Sequence point.
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[SEQUENCE_FN:%.*]] = function_ref @$s4main8sequenceyyF
// CHECK-NEXT:    apply [[SEQUENCE_FN]]()
//   Leave the function.
// CHECK-NEXT:    destroy_addr [[TUPLE]] : $*(repeat each T)
// CHECK-NEXT:    dealloc_stack [[TUPLE]] : $*(repeat each T)
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
func callCopyAndBind<each T>(args: repeat each T) {
  let result = copyIntoTuple(repeat each args)
  sequence()
}

struct Wrapper<Value> {
  let value: Value
}

// CHECK-LABEL: @$s4main17wrapTupleElementsyAA7WrapperVyxGxQp_txxQpRvzlF
// CHECK-SAME: $@convention(thin) <each T> (@pack_guaranteed Pack{repeat each T}) -> @pack_out Pack{repeat Wrapper<each T>}
func wrapTupleElements<each T>(_ value: repeat each T) -> (repeat Wrapper<each T>) {
// CHECK:         [[VALUES:%.*]] = alloc_stack [lexical] $(repeat Wrapper<each T>), let,
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LEN:%.*]] = pack_length $Pack{repeat each T}
// CHECK-NEXT:    br bb1([[ZERO]] : $Builtin.Word)
// CHECK:       bb1([[IDX:%.*]] : $Builtin.Word)
// CHECK-NEXT:    [[IDX_EQ_LEN:%.*]] = builtin "cmp_eq_Word"([[IDX]] : $Builtin.Word, [[LEN]] : $Builtin.Word) : $Builtin.Int1
// CHECK-NEXT:     cond_br [[IDX_EQ_LEN]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[INDEX:%.*]] = dynamic_pack_index [[IDX]] of $Pack{repeat Wrapper<each T>}
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[ELT_ADDR:%.*]] = tuple_pack_element_addr [[INDEX]] of [[VALUES]] : $*(repeat Wrapper<each T>) as $*Wrapper<@pack_element([[UUID]]) each T>
// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin Wrapper<@pack_element([[UUID]]) each T>.Type
// CHECK-NEXT:    [[ARG_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %1 : $*Pack{repeat each T} as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    [[ARG_COPY:%.*]] = alloc_stack $@pack_element([[UUID]]) each T
// CHECK-NEXT:    copy_addr [[ARG_ELT_ADDR]] to [init] [[ARG_COPY]] : $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[INIT:%.*]] = function_ref @$s4main7WrapperV5valueACyxGx_tcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin Wrapper<τ_0_0>.Type) -> @out Wrapper<τ_0_0>
// CHECK-NEXT:    apply [[INIT]]<@pack_element([[UUID]]) each T>([[ELT_ADDR]], [[ARG_COPY]], [[METATYPE]])
// CHECK-NEXT:    dealloc_stack [[ARG_COPY]]
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb1([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb3:
  let values = (repeat Wrapper(value: each value))

// CHECK-NEXT:    [[VALUES_COPY:%.*]] = alloc_stack $(repeat Wrapper<each T>)
// CHECK-NEXT:    copy_addr [[VALUES]] to [init] [[VALUES_COPY]] : $*(repeat Wrapper<each T>)
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LEN:%.*]] = pack_length $Pack{repeat each T}
// CHECK-NEXT:    br bb4([[ZERO]] : $Builtin.Word)
// CHECK:       bb4([[IDX:%.*]] : $Builtin.Word)
// CHECK-NEXT:    [[IDX_EQ_LEN:%.*]] = builtin "cmp_eq_Word"([[IDX]] : $Builtin.Word, [[LEN]] : $Builtin.Word) : $Builtin.Int1
// CHECK-NEXT:     cond_br [[IDX_EQ_LEN]], bb6, bb5
// CHECK:       bb5:
// CHECK-NEXT:    [[INDEX:%.*]] = dynamic_pack_index [[IDX]] of $Pack{repeat Wrapper<each T>}
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[OUT_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %0 : $*Pack{repeat Wrapper<each T>} as $*Wrapper<@pack_element([[UUID]]) each T>
// CHECK-NEXT:    [[VALUES_COPY_ELT_ADDR:%.*]] = tuple_pack_element_addr [[INDEX]] of [[VALUES_COPY]] : $*(repeat Wrapper<each T>) as $*Wrapper<@pack_element([[UUID]]) each T>
// CHECK-NEXT:   copy_addr [take] [[VALUES_COPY_ELT_ADDR]] to [init] [[OUT_ELT_ADDR]] : $*Wrapper<@pack_element([[UUID]]) each T>
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb4([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb6:
// CHECK-NEXT:    dealloc_stack [[VALUES_COPY]] :
// CHECK-NEXT:    destroy_addr [[VALUES]] :
// CHECK-NEXT:    dealloc_stack [[VALUES]] :
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
  return values
}

struct Pair<First, Second> {
  init(_ first: First, _ second: Second) {}
}

// CHECK-LABEL: @$s4main9makePairs6firsts7secondsAA4PairVyxq_GxQp_txxQp_q_xQptRvzRv_q_Rhzr0_lF
// CHECK-SAME: $@convention(thin) <each First, each Second where (repeat (each First, each Second)) : Any> (@pack_guaranteed Pack{repeat each First}, @pack_guaranteed Pack{repeat each Second}) -> @pack_out Pack{repeat Pair<each First, each Second>}
func makePairs<each First, each Second>(
  firsts first: repeat each First,
  seconds second: repeat each Second
) -> (repeat Pair<each First, each Second>) {
// CHECK:         [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LEN:%.*]] = pack_length $Pack{repeat each First}
// CHECK-NEXT:    br bb1([[ZERO]] : $Builtin.Word)
// CHECK:       bb1([[IDX:%.*]] : $Builtin.Word)
// CHECK-NEXT:    [[IDX_EQ_LEN:%.*]] = builtin "cmp_eq_Word"([[IDX]] : $Builtin.Word, [[LEN]] : $Builtin.Word) : $Builtin.Int1
// CHECK-NEXT:     cond_br [[IDX_EQ_LEN]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[INDEX:%.*]] = dynamic_pack_index [[IDX]] of $Pack{repeat Pair<each First, each Second>}
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each First, each Second where (repeat (each First, each Second)) : Any> at <Pack{repeat each First}, Pack{repeat each Second}>, shape $each First, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[OUT_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %0 : $*Pack{repeat Pair<each First, each Second>} as $*Pair<@pack_element([[UUID]]) each First, @pack_element([[UUID]]) each Second>
// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thin Pair<@pack_element([[UUID]]) each First, @pack_element([[UUID]]) each Second>.Type
// CHECK-NEXT:    [[FIRST_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %1 : $*Pack{repeat each First} as $*@pack_element([[UUID]]) each First
// CHECK-NEXT:    [[FIRST_COPY:%.*]] = alloc_stack $@pack_element([[UUID]]) each First
// CHECK-NEXT:    copy_addr [[FIRST_ELT_ADDR]] to [init] [[FIRST_COPY]] : $*@pack_element([[UUID]]) each First
// CHECK-NEXT:    [[SECOND_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %2 : $*Pack{repeat each Second} as $*@pack_element([[UUID]]) each Second
// CHECK-NEXT:    [[SECOND_COPY:%.*]] = alloc_stack $@pack_element([[UUID]]) each Second
// CHECK-NEXT:    copy_addr [[SECOND_ELT_ADDR]] to [init] [[SECOND_COPY]] : $*@pack_element([[UUID]]) each Second
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[FN:%.*]] = function_ref
// CHECK-NEXT:    [[PAIR:%.*]] = apply [[FN]]<@pack_element([[UUID]]) each First, @pack_element([[UUID]]) each Second>([[FIRST_COPY]], [[SECOND_COPY]], [[METATYPE]])
// CHECK-NEXT:    dealloc_stack [[SECOND_COPY]] : $*@pack_element([[UUID]]) each Second
// CHECK-NEXT:    dealloc_stack [[FIRST_COPY]] : $*@pack_element([[UUID]]) each First
// CHECK-NEXT:    store [[PAIR]] to [trivial] [[OUT_ELT_ADDR]] : $*Pair<@pack_element([[UUID]]) each First, @pack_element([[UUID]]) each Second>
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb1([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb3:
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
  return (repeat Pair(each first, each second))
}

protocol Container {
  associatedtype Contents
  var contents: Contents { get }
}

func makeContentsPairs<each First: Container, each Second: Container>(
  firsts first: repeat each First,
  seconds second: repeat each Second
) -> (repeat Pair<(each First).Contents, (each Second).Contents>) {
  return (repeat Pair((each first).contents, (each second).contents))
}
