// RUN: %target-swift-emit-silgen -enable-experimental-feature VariadicGenerics %s | %FileCheck %s
// REQUIRES: asserts

func takeAny(_ arg: Any) {}

// CHECK-LABEL: @$s4main16testIgnoredTupleyyxxQpRvzlF : $@convention(thin) <each T> (@pack_guaranteed Pack{repeat each T}) -> () {
// CHECK:       bb0(%0 : $*Pack{repeat each T}):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LEN:%.*]] = pack_length $Pack{repeat each T}
// CHECK-NEXT:    br bb1([[ZERO]] : $Builtin.Word)
// CHECK:       bb1([[IDX:%.*]] : $Builtin.Word)
// CHECK-NEXT:    [[IDX_EQ_LEN:%.*]] = builtin "cmp_eq_Word"([[IDX]] : $Builtin.Word, [[LEN]] : $Builtin.Word) : $Builtin.Int1
// CHECK-NEXT:     cond_br [[IDX_EQ_LEN]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[INDEX:%.*]] = dynamic_pack_index [[IDX]] of $Pack{repeat ()}
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[TEMP:%.*]] = alloc_stack $Any
// CHECK-NEXT:    [[ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %0 : $*Pack{repeat each T} as $*@pack_element([[UUID]]) T
// CHECK-NEXT:    [[TEMP_AS_T:%.*]] = init_existential_addr [[TEMP]] : $*Any, $@pack_element([[UUID]]) T
// CHECK-NEXT:    copy_addr [[ELT_ADDR]] to [init] [[TEMP_AS_T]] : $*@pack_element([[UUID]]) T
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[FN:%.*]] = function_ref @$s4main7takeAnyyyypF
// CHECK-NEXT:    apply [[FN]]([[TEMP]])
// CHECK-NEXT:    destroy_addr [[TEMP]] : $*Any
// CHECK-NEXT:    dealloc_stack [[TEMP]] : $*Any
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb1([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb3:
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()
func testIgnoredTuple<each T>(_ args: repeat each T) {
  (repeat takeAny(each args))
}

public struct Stored<Value> {}

public struct Container<each T> {
  public var storage: (repeat Stored<each T>)

  public init() {
    self.storage = (repeat Stored<each T>())
  }
}

//   Getter for Container.storage
// CHECK-LABEL: sil {{.*}}@$s4main9ContainerV7storageAA6StoredVyxGxQp_tvg
// CHECK-SAME:    $@convention(method) <each T> (@in_guaranteed Container<repeat each T>) -> @pack_out Pack{repeat Stored<each T>}
// CHECK:         [[FIELD:%.*]] = struct_element_addr %1 : $*Container<repeat each T>, #Container.storage
//   Copy the field into a temporary for some reason?
// CHECK-NEXT:    [[FIELD_COPY:%.*]] = alloc_stack $(repeat Stored<each T>)
// CHECK-NEXT:    copy_addr [[FIELD]] to [init] [[FIELD_COPY]] : $*(repeat Stored<each T>)
//   Pack loop to copy into the @pack_out parameter.
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LEN:%.*]] = pack_length $Pack{repeat each T}
// CHECK-NEXT:    br bb1([[ZERO]] : $Builtin.Word)
// CHECK:       bb1([[IDX:%.*]] : $Builtin.Word)
// CHECK-NEXT:    [[IDX_EQ_LEN:%.*]] = builtin "cmp_eq_Word"([[IDX]] : $Builtin.Word, [[LEN]] : $Builtin.Word) : $Builtin.Int1
// CHECK-NEXT:     cond_br [[IDX_EQ_LEN]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[INDEX:%.*]] = dynamic_pack_index [[IDX]] of $Pack{repeat Stored<each T>}
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[OUT_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %0 : $*Pack{repeat Stored<each T>} as $*Stored<@pack_element([[UUID]]) T>
// CHECK-NEXT:    [[FIELD_ELT_ADDR:%.*]] = tuple_pack_element_addr [[INDEX]] of [[FIELD_COPY]] : $*(repeat Stored<each T>) as $*Stored<@pack_element([[UUID]]) T>
// CHECK-NEXT:    [[ELT_VALUE:%.*]] = load [trivial] [[FIELD_ELT_ADDR]] : $*Stored<@pack_element([[UUID]]) T>
// CHECK-NEXT:    store [[ELT_VALUE]] to [trivial] [[OUT_ELT_ADDR]] : $*Stored<@pack_element([[UUID]]) T>
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb1([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb3:
//   Clean up.
// CHECK-NEXT:    dealloc_stack [[FIELD_COPY]] : $*(repeat Stored<each T>)
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()

//   Setter for Container.storage
// CHECK-LABEL: sil {{.*}}@$s4main9ContainerV7storageAA6StoredVyxGxQp_tvs
// CHECK-SAME:    $@convention(method) <each T> (@pack_owned Pack{repeat Stored<each T>}, @inout Container<repeat each T>) -> () 
//   Materialize the pack into a local tuple.
// CHECK:         [[ARG_COPY:%.*]] = alloc_stack $(repeat Stored<each T>), let,
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LEN:%.*]] = pack_length $Pack{repeat each T}
// CHECK-NEXT:    br bb1([[ZERO]] : $Builtin.Word)
// CHECK:       bb1([[IDX:%.*]] : $Builtin.Word)
// CHECK-NEXT:    [[IDX_EQ_LEN:%.*]] = builtin "cmp_eq_Word"([[IDX]] : $Builtin.Word, [[LEN]] : $Builtin.Word) : $Builtin.Int1
// CHECK-NEXT:     cond_br [[IDX_EQ_LEN]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[INDEX:%.*]] = dynamic_pack_index [[IDX]] of $Pack{repeat Stored<each T>}
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[ARG_COPY_ELT_ADDR:%.*]] = tuple_pack_element_addr [[INDEX]] of [[ARG_COPY]] : $*(repeat Stored<each T>) as $*Stored<@pack_element([[UUID]]) T>
// CHECK-NEXT:    [[PACK_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %0 : $*Pack{repeat Stored<each T>} as $*Stored<@pack_element([[UUID]]) T>
// CHECK-NEXT:    [[ELT_VALUE:%.*]] = load [trivial] [[PACK_ELT_ADDR]] : $*Stored<@pack_element([[UUID]]) T>
// CHECK-NEXT:    store [[ELT_VALUE]] to [trivial] [[ARG_COPY_ELT_ADDR]] : $*Stored<@pack_element([[UUID]]) T>
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb1([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb3:
// CHECK-NEXT:    debug_value
//   Copy the local tuple for some reason?
// CHECK-NEXT:    [[COPY2:%.*]] = alloc_stack $(repeat Stored<each T>)
// CHECK-NEXT:    copy_addr [[ARG_COPY]] to [init] [[COPY2]] : $*(repeat Stored<each T>)
//   Finally, the actual assignment.
// CHECK-NEXT:    [[ACCESS:%.*]] = begin_access [modify] [unknown] %1 :
// CHECK-NEXT:    [[FIELD:%.*]] = struct_element_addr [[ACCESS]] : $*Container<repeat each T>, #Container.storage
// CHECK-NEXT:    copy_addr [take] [[COPY2]] to [[FIELD]] : $*(repeat Stored<each T>)
// CHECK-NEXT:    end_access [[ACCESS]]
//   Clean up.
// CHECK-NEXT:    dealloc_stack [[COPY2]]
// CHECK-NEXT:    dealloc_stack [[ARG_COPY]]
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]] : $()

// CHECK-LABEL: sil {{.*}}@$s4main9ContainerV7storageAA6StoredVyxGxQp_tvM
