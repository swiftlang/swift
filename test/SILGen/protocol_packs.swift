// RUN: %target-swift-emit-silgen -target %target-swift-5.9-abi-triple -module-name test %s | %FileCheck %s

// A trivial generic type.
struct EmptyGeneric<T> {}

// A non-trivial generic type.
struct NonTrivialGeneric<T> {
  var label: String
}

public protocol PackProvider<Pack> {
  associatedtype Pack

  func withFunction<each A>(_ encode: (repeat each A) -> Void)
    where Pack == (repeat each A)
}

// #81002. Make sure we apply substitutions to the abstraction pattern when
// calling a protocol requirement with variadic generic arguments,
// because we may need them when propagating patterns down to expressions
// that are sensitive to variadic expansion --- in this case, the
// argument list of the closure.
//
// The `where` clause on the protocol requirement above isn't strictly
// necessary; we'd just need some other way to constrain the pack while
// also having a closure like this.
func testClosure<E: PackProvider<(String, Int)>>(provider: E) {
  //   Just check that we emitted the closure with a pack argument, which
  //   implies that we've done substitution into the AP and aligned the
  //   types properly.
  // CHECK-LABEL: sil private [ossa] @$s4test0A7Closure8provideryx_tAA12PackProviderRzSS_Sit0D0RtzlFySS_SitXEfU_ : $@convention(thin) @substituted <each τ_0_0> (@pack_guaranteed Pack{repeat each τ_0_0}) -> () for <Pack{String, Int}>
  provider.withFunction { (first, second) in }
}

// #80995. An assertion in reabstraction (in this case, the implicit
// reabstraction of a protocol conformance thunk) when forwarding an
// indirect tuple parameter to an expanded tuple parameter.
public protocol Schema<Value>: Sendable {
  associatedtype Value

  func encode(_ value: Value, to encoder: Encoder<Self>) throws
}

public struct Encoder<Schema> {}

private struct TupleSchema<each ElementSchema: Schema>: Schema {
  typealias Value = (repeat (each ElementSchema).Value)

  func encode(_ value: Value, to encoder: Encoder<Self>) throws {}
}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4test11TupleSchema33_6C2FE3D25D7EB8B34759FC7B1028DF25LLVyxxQp_QPGAA0C0A2aFP6encode_2toy5ValueQz_AA7EncoderVyxGtKFTW :
// CHECK-SAME:    $@convention(witness_method: Schema) <each τ_0_0 where repeat each τ_0_0 : Schema> (@in_guaranteed (repeat (each τ_0_0).Value), Encoder<TupleSchema<repeat each τ_0_0>>, @in_guaranteed TupleSchema<repeat each τ_0_0>) -> @error any Error

//   The thunk receives a tuple indirectly, but the witness receives a pack of
//   elements, so we have to initialize a pack with the tuple elements.
// CHECK:       bb0(%0 : $*(repeat (each τ_0_0).Value), %1 : $Encoder<TupleSchema<repeat each τ_0_0>>, %2 : $*TupleSchema<repeat each τ_0_0>):
// CHECK-NEXT:    [[PACK:%.*]] = alloc_pack $Pack{repeat (each τ_0_0).Value}
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LENGTH:%.*]] = pack_length $Pack{repeat each τ_0_0}
// CHECK-NEXT:    br bb1([[ZERO]])
// CHECK:       bb1([[INDEX:%.*]] : $Builtin.Word):
// CHECK-NEXT:    [[DONE:%.*]] = builtin "cmp_eq_Word"([[INDEX]], [[LENGTH]])
// CHECK-NEXT:    cond_br [[DONE]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[PACK_INDEX:%.*]] = dynamic_pack_index [[INDEX]] of $Pack{repeat (each τ_0_0).Value}
// CHECK-NEXT:    open_pack_element [[PACK_INDEX]] of <each τ_0_0 where repeat each τ_0_0 : Schema> at <Pack{repeat each τ_0_0}>, shape $each τ_0_0, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[ELT_ADDR:%.*]] = tuple_pack_element_addr [[PACK_INDEX]] of %0 as $*@pack_element([[UUID]]) (each τ_0_0).Value
// CHECK-NEXT:    pack_element_set [[ELT_ADDR]] into [[PACK_INDEX]] of [[PACK]]
// CHECK-NEXT:    [[NEXT_INDEX:%.*]] = builtin "add_Word"([[INDEX]], [[ONE]])
// CHECK-NEXT:    br bb1([[NEXT_INDEX]])

// #81600. An assertion in reabstraction (again, from a witness thunk)
// when forwarding pack parameters that aren't simple `each T`s.
protocol PackAccepter {
  func accept<each T>(values: repeat EmptyGeneric<each T>)
}

struct AccepterImpl: PackAccepter {
  func accept<each T>(values: repeat EmptyGeneric<each T>) {}
}

//   Just produce a new pack with the same contents.
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4test12AccepterImplVAA04PackB0A2aDP6accept6valuesyAA12EmptyGenericVyqd__Gqd__Qp_tRvd__lFTW :
// CHECK:         [[PACK:%.*]] = alloc_pack $Pack{repeat EmptyGeneric<each τ_0_0>}
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LENGTH:%.*]] = pack_length $Pack{repeat each τ_0_0}
// CHECK-NEXT:    br bb1([[ZERO]])
// CHECK:       bb1([[INDEX:%.*]] : $Builtin.Word):
// CHECK-NEXT:    [[DONE:%.*]] = builtin "cmp_eq_Word"([[INDEX]], [[LENGTH]])
// CHECK-NEXT:    cond_br [[DONE]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[PACK_INDEX:%.*]] = dynamic_pack_index [[INDEX]] of $Pack{repeat EmptyGeneric<each τ_0_0>}
// CHECK-NEXT:    open_pack_element [[PACK_INDEX]] of <each τ_0_0> at <Pack{repeat each τ_0_0}>, shape $each τ_0_0, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[ARG_ADDR:%.*]] = pack_element_get [[PACK_INDEX]] of %0 as $*EmptyGeneric<@pack_element([[UUID]]) each τ_0_0>
// CHECK-NEXT:    pack_element_set [[ARG_ADDR]] into [[PACK_INDEX]] of [[PACK]]
// CHECK-NEXT:    [[NEXT_INDEX:%.*]] = builtin "add_Word"([[INDEX]], [[ONE]])
// CHECK-NEXT:    br bb1([[NEXT_INDEX]])

protocol ConsumingAccepter {
  func accept<each T>(values: repeat consuming NonTrivialGeneric<each T>)
}

struct ConsumingConsumingAccepterImpl: ConsumingAccepter {
  func accept<each T>(values: repeat consuming NonTrivialGeneric<each T>) {}
}

//   This is actually just checking that consuming parameter packs
//   work in general.
// CHECK-LABEL: sil hidden [ossa] @$s4test09ConsumingB12AccepterImplV6accept6valuesyAA17NonTrivialGenericVyxGxQpn_tRvzlF :
// CHECK:         [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LENGTH:%.*]] = pack_length $Pack{repeat each T}
// CHECK-NEXT:    br bb1([[LENGTH]])
// CHECK:       bb1([[PREV_INDEX:%.*]] : $Builtin.Word):
// CHECK-NEXT:    [[DONE:%.*]] = builtin "cmp_eq_Word"([[PREV_INDEX]], [[ZERO]])
// CHECK-NEXT:    cond_br [[DONE]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[INDEX:%.*]] = builtin "sub_Word"([[PREV_INDEX]], [[ONE]])
// CHECK-NEXT:    [[PACK_INDEX:%.*]] = dynamic_pack_index [[INDEX]] of $Pack{repeat NonTrivialGeneric<each T>}
// CHECK-NEXT:    open_pack_element [[PACK_INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[ARG_ADDR:%.*]] = pack_element_get [[PACK_INDEX]] of %0 as $*NonTrivialGeneric<@pack_element([[UUID]]) each T>
// CHECK-NEXT:    destroy_addr [[ARG_ADDR]]
// CHECK-NEXT:    br bb1([[INDEX]])
// CHECK:       bb3:
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    return [[RET]]

//   Check that the witness thunk forwards the argument without a temporary.
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4test09ConsumingB12AccepterImplVAA0bC0A2aDP6accept6valuesyAA17NonTrivialGenericVyqd__Gqd__Qpn_tRvd__lFTW :
// CHECK:       bb0(%0 : $*Pack{repeat NonTrivialGeneric<each τ_0_0>}, %1 : $*ConsumingConsumingAccepterImpl):
// CHECK-NEXT:    [[PACK:%.*]] = alloc_pack $Pack{repeat NonTrivialGeneric<each τ_0_0>}
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LENGTH:%.*]] = pack_length $Pack{repeat each τ_0_0}
// CHECK-NEXT:    br bb1([[ZERO]])
// CHECK:       bb1([[INDEX:%.*]] : $Builtin.Word):
// CHECK-NEXT:    [[DONE:%.*]] = builtin "cmp_eq_Word"([[INDEX]], [[LENGTH]])
// CHECK-NEXT:    cond_br [[DONE]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[PACK_INDEX:%.*]] = dynamic_pack_index [[INDEX]] of $Pack{repeat NonTrivialGeneric<each τ_0_0>}
// CHECK-NEXT:    open_pack_element [[PACK_INDEX]] of <each τ_0_0> at <Pack{repeat each τ_0_0}>, shape $each τ_0_0, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[ARG_ADDR:%.*]] = pack_element_get [[PACK_INDEX]] of %0 as $*NonTrivialGeneric<@pack_element([[UUID]]) each τ_0_0>
// CHECK-NEXT:    pack_element_set [[ARG_ADDR]] into [[PACK_INDEX]] of [[PACK]]
// CHECK-NEXT:    [[NEXT_INDEX:%.*]] = builtin "add_Word"([[INDEX]], [[ONE]])
// CHECK-NEXT:    br bb1([[NEXT_INDEX]])
// CHECK:       bb3:
// CHECK-NEXT:    load [trivial] %1
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    function_ref
// CHECK-NEXT:    apply
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    dealloc_pack [[PACK]]
// CHECK-NEXT:    return

struct BorrowingConsumingAccepterImpl: ConsumingAccepter {
  func accept<each T>(values: repeat NonTrivialGeneric<each T>) {}
}

//   Check that the witness thunk produces a new pack with the same contents,
//   but that we destroy them in the thunk after the call.
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4test30BorrowingConsumingAccepterImplVAA0cD0A2aDP6accept6valuesyAA17NonTrivialGenericVyqd__Gqd__Qpn_tRvd__lFTW :
// CHECK:       bb0(%0 : $*Pack{repeat NonTrivialGeneric<each τ_0_0>}, %1 : $*BorrowingConsumingAccepterImpl):
// CHECK-NEXT:    [[PACK:%.*]] = alloc_pack $Pack{repeat NonTrivialGeneric<each τ_0_0>}
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LENGTH:%.*]] = pack_length $Pack{repeat each τ_0_0}
// CHECK-NEXT:    br bb1([[ZERO]])
// CHECK:       bb1([[INDEX:%.*]] : $Builtin.Word):
// CHECK-NEXT:    [[DONE:%.*]] = builtin "cmp_eq_Word"([[INDEX]], [[LENGTH]])
// CHECK-NEXT:    cond_br [[DONE]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[PACK_INDEX:%.*]] = dynamic_pack_index [[INDEX]] of $Pack{repeat NonTrivialGeneric<each τ_0_0>}
// CHECK-NEXT:    open_pack_element [[PACK_INDEX]] of <each τ_0_0> at <Pack{repeat each τ_0_0}>, shape $each τ_0_0, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[ARG_ADDR:%.*]] = pack_element_get [[PACK_INDEX]] of %0 as $*NonTrivialGeneric<@pack_element([[UUID]]) each τ_0_0>
// CHECK-NEXT:    pack_element_set [[ARG_ADDR]] into [[PACK_INDEX]] of [[PACK]]
// CHECK-NEXT:    [[NEXT_INDEX:%.*]] = builtin "add_Word"([[INDEX]], [[ONE]])
// CHECK-NEXT:    br bb1([[NEXT_INDEX]])
// CHECK:       bb3:
// CHECK-NEXT:    load [trivial] %1
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    function_ref
// CHECK-NEXT:    apply
// CHECK-NEXT:    [[RET:%.*]] = tuple ()
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LENGTH:%.*]] = pack_length $Pack{repeat each τ_0_0}
// CHECK-NEXT:    br bb4([[LENGTH]])
// CHECK:       bb4([[PREV_INDEX:%.*]] : $Builtin.Word):
// CHECK-NEXT:    [[DONE:%.*]] = builtin "cmp_eq_Word"([[PREV_INDEX]], [[ZERO]])
// CHECK-NEXT:    cond_br [[DONE]], bb6, bb5
// CHECK:       bb5:
// CHECK-NEXT:    [[INDEX:%.*]] = builtin "sub_Word"([[PREV_INDEX]], [[ONE]])
// CHECK-NEXT:    [[PACK_INDEX:%.*]] = dynamic_pack_index [[INDEX]] of $Pack{repeat NonTrivialGeneric<each τ_0_0>}
// CHECK-NEXT:    open_pack_element [[PACK_INDEX]] of <each τ_0_0> at <Pack{repeat each τ_0_0}>, shape $each τ_0_0, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[ARG_ADDR:%.*]] = pack_element_get [[PACK_INDEX]] of [[PACK]] as $*NonTrivialGeneric<@pack_element([[UUID]]) each τ_0_0>
// CHECK-NEXT:    destroy_addr [[ARG_ADDR]]
// CHECK-NEXT:    br bb4([[INDEX]])
// CHECK:       bb6:
// CHECK-NEXT:    dealloc_pack [[PACK]]
// CHECK-NEXT:    return [[RET]]

protocol BorrowingAccepter {
  func accept<each T>(values: repeat NonTrivialGeneric<each T>)
}

struct ConsumingBorrowingAccepterImpl: BorrowingAccepter {
  func accept<each T>(values: repeat consuming NonTrivialGeneric<each T>) {}
}

//   Check that the witness thunk copies the pack into a temporary.
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4test30ConsumingBorrowingAccepterImplVAA0cD0A2aDP6accept6valuesyAA17NonTrivialGenericVyqd__Gqd__Qp_tRvd__lFTW :
// CHECK:       bb0(%0 : $*Pack{repeat NonTrivialGeneric<each τ_0_0>}, %1 : $*ConsumingBorrowingAccepterImpl):
// CHECK-NEXT:    [[PACK:%.*]] = alloc_pack $Pack{repeat NonTrivialGeneric<each τ_0_0>}
// CHECK-NEXT:    [[TEMP_TUPLE:%.*]] = alloc_stack $(repeat NonTrivialGeneric<each τ_0_0>)
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LENGTH:%.*]] = pack_length $Pack{repeat each τ_0_0}
// CHECK-NEXT:    br bb1([[ZERO]])
// CHECK:       bb1([[INDEX:%.*]] : $Builtin.Word):
// CHECK-NEXT:    [[DONE:%.*]] = builtin "cmp_eq_Word"([[INDEX]], [[LENGTH]])
// CHECK-NEXT:    cond_br [[DONE]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[PACK_INDEX:%.*]] = dynamic_pack_index [[INDEX]] of $Pack{repeat NonTrivialGeneric<each τ_0_0>}
// CHECK-NEXT:    open_pack_element [[PACK_INDEX]] of <each τ_0_0> at <Pack{repeat each τ_0_0}>, shape $each τ_0_0, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[TEMP_ELT_ADDR:%.*]] = tuple_pack_element_addr [[PACK_INDEX]] of [[TEMP_TUPLE]] as $*NonTrivialGeneric<@pack_element([[UUID]]) each τ_0_0>
// CHECK-NEXT:    [[ARG_ADDR:%.*]] = pack_element_get [[PACK_INDEX]] of %0 as $*NonTrivialGeneric<@pack_element([[UUID]]) each τ_0_0>
// CHECK-NEXT:    [[BORROW:%.*]] = load_borrow [[ARG_ADDR]]
// CHECK-NEXT:    [[COPY:%.*]] = copy_value [[BORROW]]
// CHECK-NEXT:    store [[COPY]] to [init] [[TEMP_ELT_ADDR]]
// CHECK-NEXT:    pack_element_set [[TEMP_ELT_ADDR]] into [[PACK_INDEX]] of [[PACK]]
// CHECK-NEXT:    end_borrow [[BORROW]]
// CHECK-NEXT:    [[NEXT_INDEX:%.*]] = builtin "add_Word"([[INDEX]], [[ONE]])
// CHECK-NEXT:    br bb1([[NEXT_INDEX]])
// CHECK:       bb3:
// CHECK-NEXT:    load [trivial] %1
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    function_ref
// CHECK-NEXT:    apply
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    dealloc_stack [[TEMP_TUPLE]]
// CHECK-NEXT:    dealloc_pack [[PACK]]
// CHECK-NEXT:    return
