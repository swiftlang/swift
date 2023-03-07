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
