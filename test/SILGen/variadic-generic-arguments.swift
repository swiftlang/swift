// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

// CHECK-LABEL: @$s4main14receive_simpleyyxxQpRvzlF : $@convention(thin) <each T> (@pack_guaranteed Pack{repeat each T}) -> () {
// CHECK:       bb0(%0 : $*Pack{repeat each T}):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
func receive_simple<each T>(_ args: repeat each T) {}

// CHECK-LABEL: @$s4main20receive_simple_ownedyyxxQpnRvzlF : $@convention(thin) <each T> (@pack_owned Pack{repeat each T}) -> () {
// CHECK:       bb0(%0 : $*Pack{repeat each T}):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LEN:%.*]] = pack_length $Pack{repeat each T}
// CHECK-NEXT:    br bb1([[LEN]] : $Builtin.Word)
// CHECK:       bb1([[LAST_IDX:%.*]] : $Builtin.Word)
// CHECK-NEXT:    [[LAST_IDX_EQ_ZERO:%.*]] = builtin "cmp_eq_Word"([[LAST_IDX]] : $Builtin.Word, [[ZERO]] : $Builtin.Word) : $Builtin.Int1
// CHECK-NEXT:     cond_br [[LAST_IDX_EQ_ZERO]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[IDX:%.*]] = builtin "sub_Word"([[LAST_IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    [[INDEX:%.*]] = dynamic_pack_index [[IDX]] of $Pack{repeat each T}
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %0 : $*Pack{repeat each T} as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    destroy_addr [[ELT_ADDR]] : $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    br bb1([[IDX]] : $Builtin.Word)
// CHECK:       bb3:
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
func receive_simple_owned<each T>(_ args: repeat __owned each T) {}

// CHECK-LABEL: @$s4main12scalar_test01i1f1sySi_SfSStF
// CHECK:         [[PACK:%.*]] = alloc_pack $Pack{Int, Float, String}
//   Copy i into a temporary and insert that into the pack.
// CHECK-NEXT:    [[INT_TEMP:%.*]] = alloc_stack $Int
// CHECK-NEXT:    store %0 to [trivial] [[INT_TEMP]] : $*Int
// CHECK-NEXT:    [[INT_INDEX:%.*]] = scalar_pack_index 0 of $Pack{Int, Float, String}
// CHECK-NEXT:    pack_element_set [[INT_TEMP]] : $*Int into [[INT_INDEX]] of [[PACK]] : $*Pack{Int, Float, String}
//   Copy f into a temporary and insert that into the pack.
// CHECK-NEXT:    [[FLOAT_TEMP:%.*]] = alloc_stack $Float
// CHECK-NEXT:    store %1 to [trivial] [[FLOAT_TEMP]] : $*Float
// CHECK-NEXT:    [[FLOAT_INDEX:%.*]] = scalar_pack_index 1 of $Pack{Int, Float, String}
// CHECK-NEXT:    pack_element_set [[FLOAT_TEMP]] : $*Float into [[FLOAT_INDEX]] of [[PACK]] : $*Pack{Int, Float, String}
//   Copy s into a temporary and insert that into the pack.
//   TODO: do this with a borrow
// CHECK-NEXT:    [[STRING_TEMP:%.*]] = alloc_stack $String
// CHECK-NEXT:    [[STRING_COPY:%.*]] = copy_value %2 : $String
// CHECK-NEXT:    store [[STRING_COPY]] to [init] [[STRING_TEMP]] : $*String
// CHECK-NEXT:    [[STRING_INDEX:%.*]] = scalar_pack_index 2 of $Pack{Int, Float, String}
// CHECK-NEXT:    pack_element_set [[STRING_TEMP]] : $*String into [[STRING_INDEX]] of [[PACK]] : $*Pack{Int, Float, String}
//   Perform the call.
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4main14receive_simpleyyxxQpRvzlF : $@convention(thin) <each τ_0_0> (@pack_guaranteed Pack{repeat each τ_0_0}) -> ()
// CHECK-NEXT:    apply [[FN]]<Pack{Int, Float, String}>([[PACK]])
//   Clean up.
// CHECK-NEXT:    destroy_addr [[STRING_TEMP]] : $*String
// CHECK-NEXT:    dealloc_stack [[STRING_TEMP]] : $*String
// CHECK-NEXT:    dealloc_stack [[FLOAT_TEMP]] : $*Float
// CHECK-NEXT:    dealloc_stack [[INT_TEMP]] : $*Int
// CHECK-NEXT:    dealloc_pack [[PACK]] : $*Pack{Int, Float, String}
func scalar_test0(i: Int, f: Float, s: String) {
  receive_simple(i, f, s)
}

// CHECK-LABEL: @$s4main12scalar_test11i1f1sySi_SfSStF
// CHECK:         [[PACK:%.*]] = alloc_pack $Pack{Int, Float, String}
//   Copy i into a temporary and insert that into the pack.
// CHECK-NEXT:    [[INT_TEMP:%.*]] = alloc_stack $Int
// CHECK-NEXT:    store %0 to [trivial] [[INT_TEMP]] : $*Int
// CHECK-NEXT:    [[INT_INDEX:%.*]] = scalar_pack_index 0 of $Pack{Int, Float, String}
// CHECK-NEXT:    pack_element_set [[INT_TEMP]] : $*Int into [[INT_INDEX]] of [[PACK]] : $*Pack{Int, Float, String}
//   Copy f into a temporary and insert that into the pack.
// CHECK-NEXT:    [[FLOAT_TEMP:%.*]] = alloc_stack $Float
// CHECK-NEXT:    store %1 to [trivial] [[FLOAT_TEMP]] : $*Float
// CHECK-NEXT:    [[FLOAT_INDEX:%.*]] = scalar_pack_index 1 of $Pack{Int, Float, String}
// CHECK-NEXT:    pack_element_set [[FLOAT_TEMP]] : $*Float into [[FLOAT_INDEX]] of [[PACK]] : $*Pack{Int, Float, String}
//   Copy s into a temporary and insert that into the pack.
//   TODO: do this with a borrow
// CHECK-NEXT:    [[STRING_TEMP:%.*]] = alloc_stack $String
// CHECK-NEXT:    [[STRING_COPY:%.*]] = copy_value %2 : $String
// CHECK-NEXT:    store [[STRING_COPY]] to [init] [[STRING_TEMP]] : $*String
// CHECK-NEXT:    [[STRING_INDEX:%.*]] = scalar_pack_index 2 of $Pack{Int, Float, String}
// CHECK-NEXT:    pack_element_set [[STRING_TEMP]] : $*String into [[STRING_INDEX]] of [[PACK]] : $*Pack{Int, Float, String}
//   Perform the call.
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[FN:%.*]] = function_ref  @$s4main20receive_simple_ownedyyxxQpnRvzlF : $@convention(thin) <each τ_0_0> (@pack_owned Pack{repeat each τ_0_0}) -> ()
// CHECK-NEXT:    apply [[FN]]<Pack{Int, Float, String}>([[PACK]])
//   Clean up.
// CHECK-NEXT:    dealloc_stack [[STRING_TEMP]] : $*String
// CHECK-NEXT:    dealloc_stack [[FLOAT_TEMP]] : $*Float
// CHECK-NEXT:    dealloc_stack [[INT_TEMP]] : $*Int
// CHECK-NEXT:    dealloc_pack [[PACK]] : $*Pack{Int, Float, String}
func scalar_test1(i: Int, f: Float, s: String) {
  receive_simple_owned(i, f, s)
}

//   FIXME: this pack should just be forwarded
// CHECK-LABEL: @$s4main10pack_test04argsyxxQp_tRvzlF
// CHECK:       bb0(%0 : $*Pack{repeat each T}):
// CHECK:         [[PACK:%.*]] = alloc_pack $Pack{repeat each T}
// CHECK-NEXT:    [[TUPLE:%.*]] = alloc_stack $(repeat each T)
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
// CHECK-NEXT:    [[DST_ELT_ADDR:%.*]] = tuple_pack_element_addr [[INDEX]] of [[TUPLE]] : $*(repeat each T) as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    [[SRC_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %0 : $*Pack{repeat each T} as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    copy_addr [[SRC_ELT_ADDR]] to [init] [[DST_ELT_ADDR]] : $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    pack_element_set [[DST_ELT_ADDR]] : $*@pack_element([[UUID]]) each T into [[INDEX]] of [[PACK]] : $*Pack{repeat each T}
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb1([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb3:
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4main14receive_simpleyyxxQpRvzlF
// CHECK-NEXT:    apply [[FN]]<Pack{repeat each T}>([[PACK]])
// CHECK-NEXT:    destroy_addr [[TUPLE]] : $*(repeat each T)
// CHECK-NEXT:    dealloc_stack [[TUPLE]] : $*(repeat each T)
// CHECK-NEXT:    dealloc_pack [[PACK]] : $*Pack{repeat each T}
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
func pack_test0<each T>(args: repeat each T) {
  receive_simple(repeat each args)
}

// CHECK-LABEL: @$s4main10pack_test14argsyxxQp_tRvzlF
// CHECK:       bb0(%0 : $*Pack{repeat each T}):
// CHECK:         [[PACK:%.*]] = alloc_pack $Pack{repeat each T}
// CHECK-NEXT:    [[TUPLE:%.*]] = alloc_stack $(repeat each T)
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
// CHECK-NEXT:    [[DST_ELT_ADDR:%.*]] = tuple_pack_element_addr [[INDEX]] of [[TUPLE]] : $*(repeat each T) as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    [[SRC_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %0 : $*Pack{repeat each T} as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    copy_addr [[SRC_ELT_ADDR]] to [init] [[DST_ELT_ADDR]] : $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    pack_element_set [[DST_ELT_ADDR]] : $*@pack_element([[UUID]]) each T into [[INDEX]] of [[PACK]] : $*Pack{repeat each T}
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb1([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb3:
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[FN:%.*]] = function_ref  @$s4main20receive_simple_ownedyyxxQpnRvzlF : $@convention(thin) <each τ_0_0> (@pack_owned Pack{repeat each τ_0_0}) -> ()
// CHECK-NEXT:    apply [[FN]]<Pack{repeat each T}>([[PACK]])
// CHECK-NEXT:    dealloc_stack [[TUPLE]] : $*(repeat each T)
// CHECK-NEXT:    dealloc_pack [[PACK]] : $*Pack{repeat each T}
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
func pack_test1<each T>(args: repeat each T) {
  receive_simple_owned(repeat each args)
}

// CHECK-LABEL: @$s4main10pack_test24args1i1syxxQp_SiSStRvzlF
// CHECK:       bb0(%0 : $*Pack{repeat each T}, %1 : $Int, %2 : @guaranteed $String):
// CHECK:         [[PACK:%.*]] = alloc_pack $Pack{Int, repeat each T, String}
//   Int argument.
// CHECK-NEXT:    [[INT_ADDR:%.*]] = alloc_stack $Int
// CHECK-NEXT:    store %1 to [trivial] [[INT_ADDR]] : $*Int
// CHECK-NEXT:    [[INT_INDEX]] = scalar_pack_index 0 of $Pack{Int, repeat each T, String}
// CHECK-NEXT:    pack_element_set [[INT_ADDR]] : $*Int into [[INT_INDEX]] of [[PACK]] : $*Pack{Int, repeat each T, String}
//   Pack expansion argument.
//   FIXME: the elements of this pack should be borrowed
// CHECK-NEXT:    [[TUPLE:%.*]] = alloc_stack $(repeat each T)
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LEN:%.*]] = pack_length $Pack{repeat each T}
// CHECK-NEXT:    br bb1([[ZERO]] : $Builtin.Word)
// CHECK:       bb1([[IDX:%.*]] : $Builtin.Word)
// CHECK-NEXT:    [[IDX_EQ_LEN:%.*]] = builtin "cmp_eq_Word"([[IDX]] : $Builtin.Word, [[LEN]] : $Builtin.Word) : $Builtin.Int1
// CHECK-NEXT:     cond_br [[IDX_EQ_LEN]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[EXPANSION_INDEX:%.*]] = dynamic_pack_index [[IDX]] of $Pack{repeat each T}
// CHECK-NEXT:    open_pack_element [[EXPANSION_INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[INDEX:%.*]] = pack_pack_index 1, [[EXPANSION_INDEX]] of $Pack{Int, repeat each T, String}
// CHECK-NEXT:    [[DST_ELT_ADDR:%.*]] = tuple_pack_element_addr [[EXPANSION_INDEX]] of [[TUPLE]] : $*(repeat each T) as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    [[SRC_ELT_ADDR:%.*]] = pack_element_get [[EXPANSION_INDEX]] of %0 : $*Pack{repeat each T} as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    copy_addr [[SRC_ELT_ADDR]] to [init] [[DST_ELT_ADDR]] : $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    pack_element_set [[DST_ELT_ADDR]] : $*@pack_element([[UUID]]) each T into [[INDEX]] of [[PACK]] : $*Pack{Int, repeat each T, String}
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb1([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb3:
//   String argument.
// CHECK-NEXT:    [[STRING_ADDR:%.*]] = alloc_stack $String
// CHECK-NEXT:    [[STRING_COPY:%.*]] = copy_value %2 : $String
// CHECK-NEXT:    store [[STRING_COPY]] to [init] [[STRING_ADDR]] : $*String
// CHECK-NEXT:    [[STRING_INDEX:%.*]] = scalar_pack_index 2 of $Pack{Int, repeat each T, String}
// CHECK-NEXT:    pack_element_set [[STRING_ADDR]] : $*String into [[STRING_INDEX]] of [[PACK]] : $*Pack{Int, repeat each T, String}
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4main14receive_simpleyyxxQpRvzlF
// CHECK-NEXT:    apply [[FN]]<Pack{Int, repeat each T, String}>([[PACK]])
// CHECK-NEXT:    destroy_addr [[STRING_ADDR]] : $*String
// CHECK-NEXT:    dealloc_stack [[STRING_ADDR]] : $*String
// CHECK-NEXT:    destroy_addr [[TUPLE]] : $*(repeat each T)
// CHECK-NEXT:    dealloc_stack [[TUPLE]] : $*(repeat each T)
// CHECK-NEXT:    dealloc_stack [[INT_ADDR]] : $*Int
// CHECK-NEXT:    dealloc_pack [[PACK]] : $*Pack{Int, repeat each T, String}
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
func pack_test2<each T>(args: repeat each T, i: Int, s: String) {
  receive_simple(i, repeat each args, s)
}
