// RUN: %target-swift-frontend -parse-as-library -emit-silgen %s | FileCheck %s

enum TrivialUnion {
  case Foo
  case Bar(Int)
  case Bas(Int, Int)
}

class C {
  init() {}
}

enum NonTrivialUnion1 {
  case Foo
  case Bar(Int)
  case Bas(Int, C)
}

enum NonTrivialUnion2 {
  case Foo
  case Bar(C)
  case Bas(Int, C)
}

enum NonTrivialUnion3 {
  case Bar(C)
  case Bas(Int, C)
}

/* TODO: Address-only unions
enum AddressOnlyUnion<T> {
  case Foo
  case Bar(T)
  case Bas(Int, T)
}
 */

func getTrivialUnion() -> TrivialUnion { return .Foo }
func getNonTrivialUnion1() -> NonTrivialUnion1 { return .Foo }
func getNonTrivialUnion2() -> NonTrivialUnion2 { return .Foo }
func getNonTrivialUnion3() -> NonTrivialUnion3 { return .Bar(C()) }
/* TODO: Address-only unions
func getAddressOnlyUnion<T>(_: T.Type) -> AddressOnlyUnion<T> { return .Foo }
 */

// CHECK-LABEL: sil hidden @_TF15lifetime_unions19destroyUnionRValuesFT_T_ : $@convention(thin) () -> () {
func destroyUnionRValues() {
  // CHECK:   [[GET_TRIVIAL_UNION:%.*]] = function_ref @_TF15lifetime_unions15getTrivialUnionFT_OS_12TrivialUnion : $@convention(thin) () -> TrivialUnion
  // CHECK:   [[TRIVIAL_UNION:%.*]] = apply [[GET_TRIVIAL_UNION]]() : $@convention(thin) () -> TrivialUnion
  // CHECK-NOT: [[TRIVIAL_UNION]]
  getTrivialUnion()

  // CHECK:   [[GET_NON_TRIVIAL_UNION_1:%.*]] = function_ref @_TF15lifetime_unions19getNonTrivialUnion1FT_OS_16NonTrivialUnion1 : $@convention(thin) () -> @owned NonTrivialUnion1
  // CHECK:   [[NON_TRIVIAL_UNION_1:%.*]] = apply [[GET_NON_TRIVIAL_UNION_1]]() : $@convention(thin) () -> @owned NonTrivialUnion1
  // CHECK:   release_value [[NON_TRIVIAL_UNION_1]] : $NonTrivialUnion1
  getNonTrivialUnion1()

  // CHECK:   [[GET_NON_TRIVIAL_UNION_2:%.*]] = function_ref @_TF15lifetime_unions19getNonTrivialUnion2FT_OS_16NonTrivialUnion2 : $@convention(thin) () -> @owned NonTrivialUnion2
  // CHECK:   [[NON_TRIVIAL_UNION_2:%.*]] = apply [[GET_NON_TRIVIAL_UNION_2]]() : $@convention(thin) () -> @owned NonTrivialUnion2
  // CHECK:   release_value [[NON_TRIVIAL_UNION_2]] : $NonTrivialUnion2
  getNonTrivialUnion2()

  // CHECK:   [[GET_NON_TRIVIAL_UNION_3:%.*]] = function_ref @_TF15lifetime_unions19getNonTrivialUnion3FT_OS_16NonTrivialUnion3 : $@convention(thin) () -> @owned NonTrivialUnion3
  // CHECK:   [[NON_TRIVIAL_UNION_3:%.*]] = apply [[GET_NON_TRIVIAL_UNION_3]]() : $@convention(thin) () -> @owned NonTrivialUnion3
  // CHECK:   release_value [[NON_TRIVIAL_UNION_3]] : $NonTrivialUnion3
  getNonTrivialUnion3()

  /* TODO: Address-only unions
  // C/HECK:   [[GET_ADDRESS_ONLY_UNION:%.*]] = function_ref @_TF15lifetime_unions19getAddressOnlyUnionU__FMQ_GOS_16AddressOnlyUnionQ__ : $@convention(thin) <T> T.Type -> AddressOnlyUnion<T>
  // C/HECK:   [[GET_ADDRESS_ONLY_UNION_SPEC:%.*]] = specialize [[GET_ADDRESS_ONLY_UNION]] : $@convention(thin) <T> T.Type -> AddressOnlyUnion<T>, $@thin Int64.Type -> AddressOnlyUnion<Int64>, T = Int
  // C/HECK:   [[ADDRESS_ONLY_UNION_ADDR:%.*]] = alloc_stack $AddressOnlyUnion<Int64>
  // C/HECK:   apply [[GET_ADDRESS_ONLY_UNION_SPEC]]([[ADDRESS_ONLY_UNION_ADDR]]#1, {{%.*}}) : $@thin Int64.Type -> AddressOnlyUnion<Int64>
  // C/HECK:   destroy_addr [[ADDRESS_ONLY_UNION_ADDR]]#1 : $*AddressOnlyUnion<Int64>
  // C/HECK:   dealloc_stack [[ADDRESS_ONLY_UNION_ADDR]]#0 : $*@local_storage AddressOnlyUnion<Int64>
  getAddressOnlyUnion(Int)
   */
}
