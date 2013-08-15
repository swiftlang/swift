// RUN: %swift -parse-as-library -emit-silgen %s | FileCheck %s

union TrivialUnion {
  case Foo
  case Bar(Int)
  case Bas(Int, Int)
}

class C {}

union NonTrivialUnion1 {
  case Foo
  case Bar(Int)
  case Bas(Int, C)
}

union NonTrivialUnion2 {
  case Foo
  case Bar(C)
  case Bas(Int, C)
}

union NonTrivialUnion3 {
  case Bar(C)
  case Bas(Int, C)
}

union AddressOnlyUnion<T> {
  case Foo
  case Bar(T)
  case Bas(Int, T)
}

func getTrivialUnion() -> TrivialUnion { return .Foo }
func getNonTrivialUnion1() -> NonTrivialUnion1 { return .Foo }
func getNonTrivialUnion2() -> NonTrivialUnion2 { return .Foo }
func getNonTrivialUnion3() -> NonTrivialUnion3 { return .Bar(C()) }
func getAddressOnlyUnion<T>(_:T.metatype) -> AddressOnlyUnion<T> { return .Foo }

// CHECK: sil @_T15lifetime_unions19destroyUnionRValuesFT_T_ : $[thin] () -> () {
func destroyUnionRValues() {
  // CHECK:   [[GET_TRIVIAL_UNION:%.*]] = function_ref @_T15lifetime_unions15getTrivialUnionFT_OS_12TrivialUnion : $[thin] () -> TrivialUnion
  // CHECK:   [[TRIVIAL_UNION:%.*]] = apply [[GET_TRIVIAL_UNION]]() : $[thin] () -> TrivialUnion
  // CHECK-NOT: [[TRIVIAL_UNION]]
  getTrivialUnion()

  // CHECK:   [[GET_NON_TRIVIAL_UNION_1:%.*]] = function_ref @_T15lifetime_unions19getNonTrivialUnion1FT_OS_16NonTrivialUnion1 : $[thin] () -> NonTrivialUnion1
  // CHECK:   [[NON_TRIVIAL_UNION_1:%.*]] = apply [[GET_NON_TRIVIAL_UNION_1]]() : $[thin] () -> NonTrivialUnion1
  // CHECK:   switch_union [[NON_TRIVIAL_UNION_1]] : $NonTrivialUnion1,
  // CHECK:     case #NonTrivialUnion1.Bas!unionelt.1: [[RELEASE_BAS:bb[0-9]+]],
  // CHECK:     default [[DONE:bb[0-9]+]]
  // CHECK: [[RELEASE_BAS]]([[VALUE:%.*]] : $(Int64, C)):
  // CHECK:   [[VALUE_1:%.*]] = tuple_extract [[VALUE]] : $(Int64, C), 1
  // CHECK:   release [[VALUE_1]] : $C
  // CHECK:   br [[DONE]]
  // CHECK: [[DONE]]:
  getNonTrivialUnion1()

  // CHECK:   [[GET_NON_TRIVIAL_UNION_2:%.*]] = function_ref @_T15lifetime_unions19getNonTrivialUnion2FT_OS_16NonTrivialUnion2 : $[thin] () -> NonTrivialUnion2
  // CHECK:   [[NON_TRIVIAL_UNION_2:%.*]] = apply [[GET_NON_TRIVIAL_UNION_2]]() : $[thin] () -> NonTrivialUnion2
  // CHECK:   switch_union [[NON_TRIVIAL_UNION_2]] : $NonTrivialUnion2,
  // CHECK:     case #NonTrivialUnion2.Bar!unionelt.1: [[RELEASE_BAR:bb[0-9]+]],
  // CHECK:     case #NonTrivialUnion2.Bas!unionelt.1: [[RELEASE_BAS:bb[0-9]+]],
  // CHECK:     default [[DONE:bb[0-9]+]]
  // CHECK: [[RELEASE_BAR]]([[VALUE:%.*]] : $C):
  // CHECK:   release [[VALUE]] : $C
  // CHECK:   br [[DONE]]
  // CHECK: [[RELEASE_BAS]]([[VALUE:%.*]] : $(Int64, C)):
  // CHECK:   [[VALUE_1:%.*]] = tuple_extract [[VALUE]] : $(Int64, C), 1
  // CHECK:   release [[VALUE_1]] : $C
  // CHECK:   br [[DONE]]
  // CHECK: [[DONE]]:
  getNonTrivialUnion2()

  // CHECK:   [[GET_NON_TRIVIAL_UNION_3:%.*]] = function_ref @_T15lifetime_unions19getNonTrivialUnion3FT_OS_16NonTrivialUnion3 : $[thin] () -> NonTrivialUnion3
  // CHECK:   [[NON_TRIVIAL_UNION_3:%.*]] = apply [[GET_NON_TRIVIAL_UNION_3]]() : $[thin] () -> NonTrivialUnion3
  // CHECK:   switch_union [[NON_TRIVIAL_UNION_3]] : $NonTrivialUnion3,
  // CHECK:     case #NonTrivialUnion3.Bar!unionelt.1: [[RELEASE_BAR:bb[0-9]+]],
  // CHECK:     case #NonTrivialUnion3.Bas!unionelt.1: [[RELEASE_BAS:bb[0-9]+]],
  // CHECK:     default [[DONE:bb[0-9]+]]
  // CHECK: [[RELEASE_BAR]]([[VALUE:%.*]] : $C):
  // CHECK:   release %23 : $C
  // CHECK:   br [[DONE]]
  // CHECK: [[RELEASE_BAS]]([[VALUE:%.*]] : $(Int64, C)):
  // CHECK:   [[VALUE_1:%.*]] = tuple_extract [[VALUE]] : $(Int64, C), 1
  // CHECK:   release [[VALUE_1]] : $C
  // CHECK:   br [[DONE]]
  // CHECK: [[DONE]]:
  getNonTrivialUnion3()

  // CHECK:   [[GET_ADDRESS_ONLY_UNION:%.*]] = function_ref @_T15lifetime_unions19getAddressOnlyUnionU__FMQ_GOS_16AddressOnlyUnionQ__ : $[thin] <T> T.metatype -> AddressOnlyUnion<T>
  // CHECK:   [[GET_ADDRESS_ONLY_UNION_SPEC:%.*]] = specialize [[GET_ADDRESS_ONLY_UNION]] : $[thin] <T> T.metatype -> AddressOnlyUnion<T>, $[thin] Int64.metatype -> AddressOnlyUnion<Int64>, T = Int
  // CHECK:   [[ADDRESS_ONLY_UNION_ADDR:%.*]] = alloc_stack $AddressOnlyUnion<Int64>
  // CHECK:   apply [[GET_ADDRESS_ONLY_UNION_SPEC]]([[ADDRESS_ONLY_UNION_ADDR]]#1, {{%.*}}) : $[thin] Int64.metatype -> AddressOnlyUnion<Int64>
  // CHECK:   destroy_addr [[ADDRESS_ONLY_UNION_ADDR]]#1 : $*AddressOnlyUnion<Int64>
  // CHECK:   dealloc_stack [[ADDRESS_ONLY_UNION_ADDR]]#0 : $*[local_storage] AddressOnlyUnion<Int64>
  getAddressOnlyUnion(Int)
}
