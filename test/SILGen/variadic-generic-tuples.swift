// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -target %target-swift-5.9-abi-triple %s | %FileCheck %s

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
// CHECK-NEXT:    [[INDEX:%.*]] = dynamic_pack_index [[IDX]] of $Pack{/* shape: each T */ repeat ()}
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[TEMP:%.*]] = alloc_stack $Any
// CHECK-NEXT:    [[ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %0 : $*Pack{repeat each T} as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    [[TEMP_AS_T:%.*]] = init_existential_addr [[TEMP]] : $*Any, $@pack_element([[UUID]]) each T
// CHECK-NEXT:    copy_addr [[ELT_ADDR]] to [init] [[TEMP_AS_T]] : $*@pack_element([[UUID]]) each T
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
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[OUT_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %0 : $*Pack{repeat Stored<each T>} as $*Stored<@pack_element([[UUID]]) each T>
// CHECK-NEXT:    [[FIELD_ELT_ADDR:%.*]] = tuple_pack_element_addr [[INDEX]] of [[FIELD_COPY]] : $*(repeat Stored<each T>) as $*Stored<@pack_element([[UUID]]) each T>
// CHECK-NEXT:    [[ELT_VALUE:%.*]] = load [trivial] [[FIELD_ELT_ADDR]] : $*Stored<@pack_element([[UUID]]) each T>
// CHECK-NEXT:    store [[ELT_VALUE]] to [trivial] [[OUT_ELT_ADDR]] : $*Stored<@pack_element([[UUID]]) each T>
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
// CHECK:         [[ARG_COPY:%.*]] = alloc_stack [var_decl] $(repeat Stored<each T>), let,
// CHECK-NEXT:    [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK-NEXT:    [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[LEN:%.*]] = pack_length $Pack{repeat each T}
// CHECK-NEXT:    br bb1([[ZERO]] : $Builtin.Word)
// CHECK:       bb1([[IDX:%.*]] : $Builtin.Word)
// CHECK-NEXT:    [[IDX_EQ_LEN:%.*]] = builtin "cmp_eq_Word"([[IDX]] : $Builtin.Word, [[LEN]] : $Builtin.Word) : $Builtin.Int1
// CHECK-NEXT:     cond_br [[IDX_EQ_LEN]], bb3, bb2
// CHECK:       bb2:
// CHECK-NEXT:    [[INDEX:%.*]] = dynamic_pack_index [[IDX]] of $Pack{repeat Stored<each T>}
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[ARG_COPY_ELT_ADDR:%.*]] = tuple_pack_element_addr [[INDEX]] of [[ARG_COPY]] : $*(repeat Stored<each T>) as $*Stored<@pack_element([[UUID]]) each T>
// CHECK-NEXT:    [[PACK_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %0 : $*Pack{repeat Stored<each T>} as $*Stored<@pack_element([[UUID]]) each T>
// CHECK-NEXT:    [[ELT_VALUE:%.*]] = load [trivial] [[PACK_ELT_ADDR]] : $*Stored<@pack_element([[UUID]]) each T>
// CHECK-NEXT:    store [[ELT_VALUE]] to [trivial] [[ARG_COPY_ELT_ADDR]] : $*Stored<@pack_element([[UUID]]) each T>
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

struct Wrapper<Value> {
  let value: Value
}

// CHECK-LABEL: @$s4main17wrapTupleElementsyAA7WrapperVyxGxQp_txxQpRvzlF : $@convention(thin) <each T> (@pack_guaranteed Pack{repeat each T}) -> @pack_out Pack{repeat Wrapper<each T>}
func wrapTupleElements<each T>(_ value: repeat each T) -> (repeat Wrapper<each T>) {
  // CHECK: [[RETURN_VAL:%.*]] : $*Pack{repeat Wrapper<each T>}

  // CHECK: [[VAR:%.*]] = alloc_stack [lexical] [var_decl] $(repeat each T)
  let values = (repeat each value)

  // Create a temporary for the 'values' in 'each values'
  // CHECK: bb3:
  // CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $(repeat each T)
  // CHECK-NEXT: copy_addr [[VAR]] to [init] [[TEMP]] : $*(repeat each T)

  // Integer values for dynamic pack loop
  // CHECK-NEXT: [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
  // CHECK-NEXT: [[ONE:%.*]] = integer_literal $Builtin.Word, 1
  // CHECK-NEXT: [[PACK_LEN:%.*]] = pack_length $Pack{repeat each T}
  // CHECK-NEXT: br bb4([[ZERO]] : $Builtin.Word)

  // Loop condition
  // CHECK: bb4([[INDEX:%.*]] : $Builtin.Word)
  // CHECK-NEXT: [[INDEX_EQ_LEN:%.*]] = builtin "cmp_eq_Word"([[INDEX]] : $Builtin.Word, [[PACK_LEN]] : $Builtin.Word) : $Builtin.Int1
  // CHECK-NEXT: cond_br [[INDEX_EQ_LEN]], bb6, bb5

  // Loop body
  // CHECK: bb5:
  // CHECK-NEXT: [[CUR_INDEX:%.*]] = dynamic_pack_index [[INDEX]] of $Pack{repeat Wrapper<each T>}
  // CHECK-NEXT: open_pack_element [[CUR_INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid [[UUID:".*"]]
  // CHECK-NEXT: [[RETURN_VAL_ELT_ADDR:%.*]] = pack_element_get [[CUR_INDEX]] of [[RETURN_VAL]] : $*Pack{repeat Wrapper<each T>} as $*Wrapper<@pack_element([[UUID]]) each T>
  // CHECK-NEXT: [[METATYPE:%.*]] = metatype $@thin Wrapper<@pack_element([[UUID]]) each T>.Type
  // CHECK-NEXT: [[TUPLE_ELT_ADDR:%.*]] = tuple_pack_element_addr [[CUR_INDEX]] of [[TEMP]] : $*(repeat each T) as $*@pack_element([[UUID]]) each T
  // CHECK-NEXT: [[INIT_ARG:%.*]] = alloc_stack $@pack_element([[UUID]]) each T
  // CHECK-NEXT: copy_addr [[TUPLE_ELT_ADDR]] to [init] [[INIT_ARG]] : $*@pack_element([[UUID]]) each T
  // function_ref Wrapper.init(value:)
  // CHECK: [[INIT:%.*]] = function_ref @$s4main7WrapperV5valueACyxGx_tcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin Wrapper<τ_0_0>.Type) -> @out Wrapper<τ_0_0>
  // CHECK-NEXT: apply [[INIT]]<@pack_element([[UUID]]) each T>([[RETURN_VAL_ELT_ADDR]], [[INIT_ARG]], [[METATYPE]]) : $@convention(method) <τ_0_0> (@in τ_0_0, @thin Wrapper<τ_0_0>.Type) -> @out Wrapper<τ_0_0>
  // CHECK-NEXT: dealloc_stack [[INIT_ARG]] : $*@pack_element([[UUID]]) each T
  // CHECK-NEXT: [[NEXT_INDEX:%.*]] = builtin "add_Word"([[INDEX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
  // CHECK-NEXT: br bb4([[NEXT_INDEX]] : $Builtin.Word)

  return (repeat Wrapper(value: each values))

  // CHECK: destroy_addr [[TEMP]] : $*(repeat each T)
  // CHECK: dealloc_stack [[TEMP]] : $*(repeat each T)
  // CHECK: destroy_addr [[VAR]] : $*(repeat each T)
  // CHECK: dealloc_stack [[VAR]] : $*(repeat each T)
  // CHECK-NEXT:    [[RET:%.*]] = tuple ()
  // CHECK-NEXT:    return [[RET]] : $()
}

// CHECK-LABEL: sil hidden [ossa] @$s4main20projectTupleElementsyyAA7WrapperVyxGxQpRvzlF : $@convention(thin) <each T> (@pack_guaranteed Pack{repeat Wrapper<each T>}) -> () {
func projectTupleElements<each T>(_ value: repeat Wrapper<each T>) {
  // CHECK: [[VAR:%.*]] = alloc_stack [lexical] [var_decl] $(repeat each T)

  // CHECK-NEXT: [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
  // CHECK-NEXT: [[ONE:%.*]] = integer_literal $Builtin.Word, 1
  // CHECK-NEXT: [[PACK_LEN:%.*]] = pack_length $Pack{repeat each T}

  // CHECK-NEXT: br bb1([[ZERO]] : $Builtin.Word)

  // CHECK: bb1([[INDEX:%.*]] : $Builtin.Word):
  // CHECK-NEXT: [[INDEX_EQ_LEN:%.*]] = builtin "cmp_eq_Word"([[INDEX]] : $Builtin.Word, [[PACK_LEN]] : $Builtin.Word)
  // CHECK-NEXT: cond_br [[INDEX_EQ_LEN]], bb3, bb2

  // CHECK: bb2:
  // CHECK-NEXT: [[CUR_INDEX:%.*]] = dynamic_pack_index [[INDEX]] of $Pack{repeat each T}
  // CHECK-NEXT: open_pack_element [[CUR_INDEX]] of <each T> at <Pack{repeat each T}>, shape $each T, uuid "[[UUID:[0-9A-F-]*]]"
  // CHECK-NEXT: [[TUPLE_ELT_ADDR:%.*]] = tuple_pack_element_addr [[CUR_INDEX]] of [[VAR]] : $*(repeat each T) as $*@pack_element("[[UUID]]") each T
  // CHECK-NEXT: [[VAL_ELT_ADDR:%.*]] = pack_element_get [[CUR_INDEX]] of %0 : $*Pack{repeat Wrapper<each T>} as $*Wrapper<@pack_element("[[UUID]]") each T>
  // CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $Wrapper<@pack_element("[[UUID]]") each T>
  // CHECK-NEXT: copy_addr [[VAL_ELT_ADDR]] to [init] [[TEMP]] : $*Wrapper<@pack_element("[[UUID]]") each T>
  // CHECK-NEXT: [[MEMBER:%.*]] = struct_element_addr [[TEMP]] : $*Wrapper<@pack_element("[[UUID]]") each T>, #Wrapper.value
  // CHECK-NEXT: copy_addr [[MEMBER]] to [init] [[TUPLE_ELT_ADDR]] : $*@pack_element("[[UUID]]") each T
  // CHECK-NEXT: destroy_addr [[TEMP]] : $*Wrapper<@pack_element("[[UUID]]") each T>
  // CHECK-NEXT: dealloc_stack [[TEMP]] : $*Wrapper<@pack_element("[[UUID]]") each T>
  // CHECK-NEXT: [[NEXT_INDEX:%.*]] = builtin "add_Word"([[INDEX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
  // CHECK-NEXT: br bb1([[NEXT_INDEX]] : $Builtin.Word)

  // CHECK: bb3:
  // CHECK-NEXT: destroy_addr [[VAR]] : $*(repeat each T)
  // CHECK-NEXT: dealloc_stack [[VAR]] : $*(repeat each T)
  // CHECK-NEXT: [[RET:%.*]] = tuple ()
  // CHECK-NEXT: return [[RET]] : $()

  let tuple = (repeat (each value).value)
}

func takesVariadicTuple<each T>(tuple: (repeat each T)) {}

// CHECK-LABEL: sil{{.*}} @$s4main28testConcreteVariadicTupleArg1i1sySi_SStF :
// CHECK:       [[PACK:%.*]] = alloc_pack $Pack{Int, String}
// CHECK-NEXT:  [[I_COPY:%.*]] = alloc_stack $Int
// CHECK-NEXT:  store %0 to [trivial] [[I_COPY]] : $*Int
// CHECK-NEXT:  [[I_INDEX:%.*]] = scalar_pack_index 0 of $Pack{Int, String}
// CHECK-NEXT:  pack_element_set [[I_COPY]] : $*Int into [[I_INDEX]] of [[PACK]] :
// CHECK-NEXT:  [[S_COPY:%.*]] = alloc_stack $String
// CHECK-NEXT:  [[T0:%.*]] = copy_value %1 : $String
// CHECK-NEXT:  store [[T0]] to [init] [[S_COPY]] : $*String
// CHECK-NEXT:  [[S_INDEX:%.*]] = scalar_pack_index 1 of $Pack{Int, String}
// CHECK-NEXT:  pack_element_set [[S_COPY]] : $*String into [[S_INDEX]] of [[PACK]] :
// CHECK-NEXT:  // function_ref
// CHECK-NEXT:  [[FN:%.*]] = function_ref @$s4main18takesVariadicTuple5tupleyxxQp_t_tRvzlF : $@convention(thin) <each τ_0_0> (@pack_guaranteed Pack{repeat each τ_0_0}) -> ()
// CHECK-NEXT:  apply [[FN]]<Pack{Int, String}>([[PACK]])
// CHECK-NEXT:  destroy_addr [[S_COPY]] :
// CHECK-NEXT:  dealloc_stack [[S_COPY]] :
// CHECK-NEXT:  dealloc_stack [[I_COPY]] :
// CHECK-NEXT:  dealloc_pack [[PACK]] :
func testConcreteVariadicTupleArg(i: Int, s: String) {
  takesVariadicTuple(tuple: (i, s))
}

struct TupleHolder<each T> {
  var content: (repeat each T)

  // Suppress the memberwise initializer
  init(values: repeat each T) {
    content = (repeat each values)
  }
}

// CHECK-LABEL: sil{{.*}} @$s4main31takesConcreteTupleHolderFactory7factoryyAA0dE0VySi_SSQPGyXE_tF :
// CHECK-SAME:    $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> @owned TupleHolder<Int, String>) -> ()
// CHECK:       [[T0:%.*]] = copy_value %0 :
// CHECK:       [[T1:%.*]] = begin_borrow [[T0]]
// CHECK:       [[RESULT:%.*]] = apply [[T1]]() :
// CHECK:       [[MOVE:%.*]] = move_value [var_decl] [[RESULT]]
// CHECK:       destroy_value [[MOVE]]
func takesConcreteTupleHolderFactory(factory: () -> TupleHolder<Int, String>) {
  let holder = factory()
}

struct MemberwiseTupleHolder<each T> {
  var content: (repeat each T)
}

//   Memberwise initializer.
//   TODO: initialize directly into the fields
// CHECK-LABEL: sil{{.*}} @$s4main21MemberwiseTupleHolderV7contentACyxxQp_QPGxxQp_t_tcfC
// CHECK-SAME:    $@convention(method) <each T> (@pack_owned Pack{repeat each T}, @thin MemberwiseTupleHolder<repeat each T>.Type) -> @out MemberwiseTupleHolder<repeat each T> {
// CHECK:         [[TEMP:%.*]] = alloc_stack $(repeat each T)
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
// CHECK-NEXT:    [[TUPLE_ELT_ADDR:%.*]] = tuple_pack_element_addr [[INDEX]] of [[TEMP]] : $*(repeat each T) as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    [[PACK_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %1 : $*Pack{repeat each T} as $*@pack_element([[UUID]]) each T
// CHECK-NEXT:    copy_addr [take] [[PACK_ELT_ADDR]] to [init] [[TUPLE_ELT_ADDR]]
// CHECK-NEXT:    [[NEXT_IDX:%.*]] = builtin "add_Word"([[IDX]] : $Builtin.Word, [[ONE]] : $Builtin.Word) : $Builtin.Word
// CHECK-NEXT:    br bb1([[NEXT_IDX]] : $Builtin.Word)
// CHECK:       bb3:
// CHECK-NEXT:    [[CONTENTS_ADDR:%.*]] = struct_element_addr %0 : $*MemberwiseTupleHolder<repeat each T>, #MemberwiseTupleHolder.content
// CHECK-NEXT:    copy_addr [take] [[TEMP]] to [init] [[CONTENTS_ADDR]]
// CHECK-NEXT:    tuple ()
// CHECK-NEXT:    dealloc_stack [[TEMP]]
// CHECK-NEXT:    return


func callVariadicMemberwiseInit() -> MemberwiseTupleHolder<Int, String> {
  return MemberwiseTupleHolder(content: (0, "hello"))
}

@_eagerMove struct MyString {
  var guts: AnyObject
}

func callVariadicMemberwiseInit(_ ms: MyString) -> MemberwiseTupleHolder<Int, MyString> {
  return MemberwiseTupleHolder(content: (0, ms))
}

// rdar://107151145: when we tuple-destructure a black hole
// initialization, the resulting element initializations need to
// handle pack expansion initialization
struct EmptyContainer<each T> {}
func f<each T>(_: repeat each T) {
  let _ = (repeat EmptyContainer<each T>())
}

// rdar://107161241: handle receiving tuples that originally contained
// packs that are not emitted into an initialization
struct FancyTuple<each T> {
  var x: (repeat each T)

  func makeTuple() -> (repeat each T) {
    return (repeat each x)
  }
}

// CHECK: sil{{.*}} @$s4main23testFancyTuple_concreteyyF :
//   Create a pack to receive the results from makeTuple.
// CHECK:      [[PACK:%.*]] = alloc_pack $Pack{Int, String, Bool}
// CHECK-NEXT: [[INT_ADDR:%.*]] = alloc_stack $Int
// CHECK-NEXT: [[INT_INDEX:%.*]] = scalar_pack_index 0 of $Pack{Int, String, Bool}
// CHECK-NEXT: pack_element_set [[INT_ADDR]] : $*Int into [[INT_INDEX]] of [[PACK]] : $*Pack{Int, String, Bool}
// CHECK-NEXT: [[STRING_ADDR:%.*]] = alloc_stack $String
// CHECK-NEXT: [[STRING_INDEX:%.*]] = scalar_pack_index 1 of $Pack{Int, String, Bool}
// CHECK-NEXT: pack_element_set [[STRING_ADDR]] : $*String into [[STRING_INDEX]] of [[PACK]] : $*Pack{Int, String, Bool}
// CHECK-NEXT: [[BOOL_ADDR:%.*]] = alloc_stack $Bool
// CHECK-NEXT: [[BOOL_INDEX:%.*]] = scalar_pack_index 2 of $Pack{Int, String, Bool}
// CHECK-NEXT: pack_element_set [[BOOL_ADDR]] : $*Bool into [[BOOL_INDEX]] of [[PACK]] : $*Pack{Int, String, Bool}
// CHECK:      [[FN:%.*]] = function_ref @$s4main10FancyTupleV04makeC0xxQp_tyF
// CHECK-NEXT: apply [[FN]]<Pack{Int, String, Bool}>([[PACK]], {{.*}})
func testFancyTuple_concrete() {
  FancyTuple<Int, String, Bool>(x: (1, "hi", false)).makeTuple()
}

func testFancyTuple_pack<each T>(values: repeat each T) {
  FancyTuple<Int, String, repeat each T, Bool>(x: (1, "hi", repeat each values, false)).makeTuple()
}

// rdar://107664237
func f<each T>() -> (repeat Array<each T>) {}

// rdar://109911655
struct GenericButLoadable<X, Y> { }
struct StructOfLoadableTuple<each S> {
  let elements: (repeat GenericButLoadable<each S, each S>)
}
// Force the emission of the memberwise initializer.
func testStructOfLoadableTuple() -> StructOfLoadableTuple<Int> {
  StructOfLoadableTuple(elements: (GenericButLoadable<Int, Int>()))
}

//   The memberwise initializer.
// CHECK-LABEL: sil {{.*}}@$s4main21StructOfLoadableTupleV8elementsACyxxQp_QPGAA010GenericButD0VyxxGxQp_t_tcfC :
// CHECK:       bb0(%0 : $*StructOfLoadableTuple<repeat each S>, %1 : $*Pack{repeat GenericButLoadable<each S, each S>}, %2 : $@thin StructOfLoadableTuple<repeat each S>.Type):
// CHECK-NEXT:    [[TUPLE:%.*]] = alloc_stack $(repeat GenericButLoadable<each S, each S>)
// CHECK:         [[INDEX:%.*]] = dynamic_pack_index {{.*}} of $Pack{repeat GenericButLoadable<each S, each S>}
// CHECK-NEXT:    open_pack_element [[INDEX]] of <each S> at <Pack{repeat each S}>, shape $each S, uuid [[UUID:".*"]]
// CHECK-NEXT:    [[TUPLE_ELT_ADDR:%.*]] = tuple_pack_element_addr [[INDEX]] of [[TUPLE]] : $*(repeat GenericButLoadable<each S, each S>) as $*GenericButLoadable<@pack_element([[UUID]]) each S, @pack_element([[UUID]]) each S>
// CHECK-NEXT:    [[PACK_ELT_ADDR:%.*]] = pack_element_get [[INDEX]] of %1 : $*Pack{repeat GenericButLoadable<each S, each S>} as $*GenericButLoadable<@pack_element([[UUID]]) each S, @pack_element([[UUID]]) each S>
// CHECK-NEXT:    [[PACK_ELT:%.*]] = load [trivial] [[PACK_ELT_ADDR]] :
// CHECK-NEXT:    store [[PACK_ELT]] to [trivial] [[TUPLE_ELT_ADDR]] :

// rdar://107290521
//   The verifier had some home-grown type-lowering logic that didn't
//   know about pack expansions.
// CHECK-LABEL: sil {{.*}}@$s4main22testExistentialErasureyyxxQpRvzlF1gL_yyqd__qd__QpRvzRvd__r__lF :
// CHECK:         [[T0:%.*]] = init_existential_addr {{.*}} : $*Any, $(repeat (each T).Type)
// CHECK:         tuple_pack_element_addr {{.*}} of [[T0]] : $*(repeat @thick (each T).Type) as $*@thick (@pack_element("{{.*}}") each T).Type
func testExistentialErasure<each T>(_: repeat each T) {
  func g<each U>(_: repeat each U) {
    print((repeat (each T).self))
    print((repeat (each U).self))
  }

  g(1, "hi", false)
}

// Issue #70187
func identityOnVariadicTuples<each T>(_ value: (repeat each T)) -> (repeat each T) {
  (repeat each value)
}

func testPassReturnedVariadicTuple() {
  takesVariadicTuple(tuple: identityOnVariadicTuples((1, 2, 3)))
}
// CHECK-LABEL: sil {{.*}}@$s4main29testPassReturnedVariadicTupleyyF :
// CHECK:         [[RESULT_PACK:%.*]] = alloc_pack $Pack{Int, Int, Int}
// CHECK-NEXT:    [[E0_ADDR:%.*]] = alloc_stack $Int
// CHECK-NEXT:    [[I0:%.*]] = scalar_pack_index 0 of $Pack{Int, Int, Int}
// CHECK-NEXT:    pack_element_set [[E0_ADDR]] : $*Int into [[I0]] of [[RESULT_PACK]] :
// CHECK-NEXT:    [[E1_ADDR:%.*]] = alloc_stack $Int
// CHECK-NEXT:    [[I1:%.*]] = scalar_pack_index 1 of $Pack{Int, Int, Int}
// CHECK-NEXT:    pack_element_set [[E1_ADDR]] : $*Int into [[I1]] of [[RESULT_PACK]] :
// CHECK-NEXT:    [[E2_ADDR:%.*]] = alloc_stack $Int
// CHECK-NEXT:    [[I2:%.*]] = scalar_pack_index 2 of $Pack{Int, Int, Int}
// CHECK-NEXT:    pack_element_set [[E2_ADDR]] : $*Int into [[I2]] of [[RESULT_PACK]] :
// CHECK-NEXT:    [[ARG_PACK:%.*]] = alloc_pack $Pack{Int, Int, Int}
// CHECK:         apply {{.*}}<Pack{Int, Int, Int}>([[RESULT_PACK]], [[ARG_PACK]])
// CHECK:         [[E0:%.*]] = load [trivial] [[E0_ADDR]] : $*Int
// CHECK-NEXT:    [[E1:%.*]] = load [trivial] [[E1_ADDR]] : $*Int
// CHECK-NEXT:    [[E2:%.*]] = load [trivial] [[E2_ADDR]] : $*Int
// CHECK-NEXT:    [[ARG_PACK2:%.*]] = alloc_pack $Pack{Int, Int, Int}

func test() {
  let tuple = identityOnVariadicTuples((1, 2, 3))
  takesVariadicTuple(tuple: tuple)
}

func createTuple<each T>(including: repeat Stored<each T>, from: Int) -> (repeat each T) {
  fatalError()
}

// rdar://121489308
func testTupleExpansionInEnumConstructor<each T>(
  from: repeat Stored<each T>,
  to: @escaping (Result<(repeat each T), Error>) -> ()
) {
  _ = {
    let tuple = createTuple(including: repeat each from,
                            from: 42)
    to(.success(tuple))
  }
}
// CHECK-LABEL: sil {{.*}}@$s4main35testTupleExpansionInEnumConstructor4from2toyAA6StoredVyxGxQp_ys6ResultOyxxQp_ts5Error_pGctRvzlFyycfU_ :
// CHECK:         [[VAR:%.*]] = alloc_stack [lexical] [var_decl] $(repeat each T), let, name "tuple"
//   (a few moments later)
// CHECK:         metatype $@thin Result<(repeat each T), any Error>.Type
// CHECK:         [[RESULT_TEMP:%.*]] = alloc_stack $Result<(repeat each T), any Error>
// CHECK-NEXT:    [[PAYLOAD_ADDR:%.*]] = init_enum_data_addr [[RESULT_TEMP]] : $*Result<(repeat each T), any Error>, #Result.success
// CHECK-NEXT:    copy_addr [[VAR]] to [init] [[PAYLOAD_ADDR]] : $*(repeat each T)
// CHECK-NEXT:    inject_enum_addr [[RESULT_TEMP]] : $*Result<(repeat each T), any Error>, #Result.success
