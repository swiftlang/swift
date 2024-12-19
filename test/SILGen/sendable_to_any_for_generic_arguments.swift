// RUN: %target-swift-emit-silgen %s -verify | %FileCheck %s

class User {
  @preconcurrency var dict: [String : any Sendable] = [:]
  @preconcurrency var arr: [any Sendable] = []
  // Note: No Set because `any Sendable` is not Hashable
}

extension Dictionary where Key == String, Value == Any {
  func onlyWhenValueAny() {}
}

extension Array where Element == Any {
  func onlyWhenValueAny() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s37sendable_to_any_for_generic_arguments31test_conditional_on_collections1uyAA4UserC_tF
// CHECK: unchecked_value_cast {{.*}} to $Dictionary<String, Any>
// CHECK: unchecked_value_cast {{.*}} to $Array<Any>
func test_conditional_on_collections(u: User) {
  u.dict.onlyWhenValueAny()
  u.arr.onlyWhenValueAny()
}

// Check that `any Sendable` extension is preferred.

extension Dictionary where Key == String, Value == Any {
  func noAmbiguity() {}
}

extension Array where Element == Any {
  func noAmbiguity() {}
}

extension Dictionary where Key == String, Value == any Sendable {
  func noAmbiguity() {}
}

extension Array where Element == any Sendable {
  func noAmbiguity() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s37sendable_to_any_for_generic_arguments41test_no_ambiguity_with_Sendable_extension1uyAA4UserC_tF
// CHECK-NOT: unchecked_value_cast {{.*}}
// CHECK: [[DICT_METHOD_REF:%.*]] = function_ref @$sSD37sendable_to_any_for_generic_argumentsSSRszs8Sendable_pRs_rlE11noAmbiguityyyF : $@convention(method) (@guaranteed Dictionary<String, any Sendable>) -> ()
// CHECK-NEXT: {{.*}} = apply [[DICT_METHOD_REF]]({{.*}}) : $@convention(method) (@guaranteed Dictionary<String, any Sendable>) -> ()
// CHECK-NOT: unchecked_value_cast {{.*}}
// CHECK: [[ARR_METHOD_REF:%.*]] = function_ref @$sSa37sendable_to_any_for_generic_argumentss8Sendable_pRszlE11noAmbiguityyyF : $@convention(method) (@guaranteed Array<any Sendable>) -> ()
// CHECK-NEXT: {{.*}} = apply [[ARR_METHOD_REF]]({{.*}}) : $@convention(method) (@guaranteed Array<any Sendable>) -> ()
func test_no_ambiguity_with_Sendable_extension(u: User) {
  u.dict.noAmbiguity()
  u.arr.noAmbiguity()
}

struct S<T> {
}

extension S where T == Any {
  func anyOnly() {}
}

struct TestGeneral {
  @preconcurrency var v: S<any Sendable>
  @preconcurrency var optV: S<[(any Sendable)?]>
  @preconcurrency var funcV: S<([any Sendable]) -> (any Sendable)?>

  func accepts_any(_: S<Any>) {}
  func accepts_opt_any(_: S<[Any?]>) {}
  func sameType<T>(_: S<T>, _: T.Type) {}

  // CHECK-LABEL: sil hidden [ossa] @$s37sendable_to_any_for_generic_arguments11TestGeneralV15test_contextualAA1SVyypGyF
  // CHECK: [[V_REF:%.*]] = struct_extract %0, #TestGeneral.v
  // CHECK-NEXT: {{.*}} = unchecked_trivial_bit_cast [[V_REF]] to $S<Any>
  func test_contextual() -> S<Any> {
    v
  }

  // CHECK-LABEL: sil hidden [ossa] @$s37sendable_to_any_for_generic_arguments11TestGeneralV15test_member_refyyF
  // CHECK: [[V_REF:%.*]] = struct_extract %0, #TestGeneral.v
  // CHECK-NEXT: [[V_WITH_ANY:%.*]] = unchecked_trivial_bit_cast [[V_REF:%.*]] to $S<Any>
  // CHECK: [[MEMBER_REF:%.*]] = function_ref @$s37sendable_to_any_for_generic_arguments1SVAAypRszlE0C4OnlyyyF : $@convention(method) (S<Any>) -> ()
  // CHECK-NEXT: {{.*}} = apply [[MEMBER_REF]]([[V_WITH_ANY:%.*]]) : $@convention(method) (S<Any>) -> ()
  func test_member_ref() {
    v.anyOnly()
  }

  // CHECK-LABEL: sil hidden [ossa] @$s37sendable_to_any_for_generic_arguments11TestGeneralV24test_passing_as_argumentyyF
  // CHECK: [[V_REF:%.*]] = struct_extract %0, #TestGeneral.v
  // CHECK-NEXT: [[V_ANY:%.*]] = unchecked_trivial_bit_cast [[V_REF]] to $S<Any>
  // CHECK: [[MEMBER_REF:%.*]] = function_ref @$s37sendable_to_any_for_generic_arguments11TestGeneralV08accepts_C0yyAA1SVyypGF : $@convention(method) (S<Any>, TestGeneral) -> ()
  // CHECK-NEXT: {{.*}} = apply [[MEMBER_REF]]([[V_ANY]], %0) : $@convention(method) (S<Any>, TestGeneral) -> ()
  func test_passing_as_argument() {
    accepts_any(v)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s37sendable_to_any_for_generic_arguments11TestGeneralV39test_passing_as_argument_through_member1tyAC_tF
  // CHECK: [[V_REF:%.*]] = struct_extract %0, #TestGeneral.v
  // CHECK-NEXT: [[V_ANY:%.*]] = unchecked_trivial_bit_cast [[V_REF]] to $S<Any>
  // CHECK: [[MEMBER_REF:%.*]] = function_ref @$s37sendable_to_any_for_generic_arguments11TestGeneralV08accepts_C0yyAA1SVyypGF : $@convention(method) (S<Any>, TestGeneral) -> ()
  // CHECK-NEXT: {{.*}} = apply [[MEMBER_REF]]([[V_ANY]], %1) : $@convention(method) (S<Any>, TestGeneral) -> ()
  func test_passing_as_argument_through_member(t: TestGeneral) {
    accepts_any(t.v)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s37sendable_to_any_for_generic_arguments11TestGeneralV23test_complex_contextualAA1SVySayypSgGGyF
  // CHECK: [[V_REF:%.*]] = struct_extract %0, #TestGeneral.optV
  // CHECK-NEXT: {{.*}} = unchecked_trivial_bit_cast [[V_REF]] to $S<Array<Optional<Any>>>
  func test_complex_contextual() -> S<[Any?]> {
    optV
  }

  // CHECK-LABEL: sil hidden [ossa] @$s37sendable_to_any_for_generic_arguments11TestGeneralV26test_complex_with_argument1tyAC_tF
  // CHECK: [[SELF_V_REF:%.*]] = struct_extract %1, #TestGeneral.optV
  // CHECK-NEXT: [[SELF_V_ANY:%.*]] = unchecked_trivial_bit_cast [[SELF_V_REF]] to $S<Array<Optional<Any>>>
  // CHECK: [[MEMBER_REF_1:%.*]] = function_ref @$s37sendable_to_any_for_generic_arguments11TestGeneralV012accepts_opt_C0yyAA1SVySayypSgGGF : $@convention(method) (S<Array<Optional<Any>>>, TestGeneral) -> ()
  // CHECK-NEXT: {{.*}} = apply [[MEMBER_REF_1]]([[SELF_V_ANY]], %1) : $@convention(method) (S<Array<Optional<Any>>>, TestGeneral) -> ()
  // CHECK: [[V_REF_ON_T:%.*]] = struct_extract %0, #TestGeneral.optV
  // CHECK-NEXT: [[V_ANY_ON_T:%.]] = unchecked_trivial_bit_cast [[V_REF_ON_T]] to $S<Array<Optional<Any>>>
  // CHECK: [[MEMBER_REF_2:%.*]] = function_ref @$s37sendable_to_any_for_generic_arguments11TestGeneralV012accepts_opt_C0yyAA1SVySayypSgGGF : $@convention(method) (S<Array<Optional<Any>>>, TestGeneral) -> ()
  // CHECK-NEXT: {{.*}} = apply [[MEMBER_REF_2]]([[V_ANY_ON_T]], %1) : $@convention(method) (S<Array<Optional<Any>>>, TestGeneral) -> ()
  func test_complex_with_argument(t: TestGeneral) {
    accepts_opt_any(optV)
    accepts_opt_any(t.optV)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s37sendable_to_any_for_generic_arguments11TestGeneralV14test_same_typeyyF
  // CHECK: [[V_REF:%.*]] = struct_extract %0, #TestGeneral.v
  // CHECK-NEXT: [[V_ANY:%.*]] = unchecked_trivial_bit_cast [[V_REF]] to $S<Any>
  // CHECK: [[ANY_METATYPE:%.*]] = metatype $@thick (any Any).Type
  // CHECK: [[SAME_TYPE_FN:%.*]] = function_ref @$s37sendable_to_any_for_generic_arguments11TestGeneralV8sameTypeyyAA1SVyxG_xmtlF : $@convention(method) <τ_0_0> (S<τ_0_0>, @thick τ_0_0.Type, TestGeneral) -> ()
  // CHECK-NEXT: %7 = apply %6<Any>([[V_ANY]], [[ANY_METATYPE]], %0)
  func test_same_type() {
    sameType(v, Any.self)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s37sendable_to_any_for_generic_arguments11TestGeneralV18test_address_castsyyF
  // CHECK: [[DATA_REF:%.*]] = struct_element_addr %2, #<abstract function>Test.data
  // CHECK-NEXT: [[DATA_COPY:%.*]] = alloc_stack $V<any Sendable>
  // CHECK-NEXT: copy_addr [[DATA_REF]] to [init] [[DATA_COPY]]
  // CHECK-NEXT: [[DATA_ANY:%.*]] = unchecked_addr_cast [[DATA_COPY]] to $*V<Any>
  // CHECK: [[ACCEPTS_ANY:%.*]] = function_ref @$s37sendable_to_any_for_generic_arguments11TestGeneralV18test_address_castsyyF08accepts_C0L_yyAcDyyF1VL_VyypGF : $@convention(thin) (@in_guaranteed V<Any>) -> ()
  // CHECK-NEXT: {{.*}} = apply [[ACCEPTS_ANY]]([[DATA_ANY]]) : $@convention(thin) (@in_guaranteed V<Any>) -> ()
  func test_address_casts() {
    struct V<T> {
      let v: T
    }

    struct Test {
      @preconcurrency var data: V<any Sendable>
    }


    func accepts_any(_: V<Any>) {}

    let test = Test(data: V(v: 42))
    accepts_any(test.data)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s37sendable_to_any_for_generic_arguments11TestGeneralV023test_function_types_as_E5_argsyyF
  // CHECK: [[FUNCV_REF:%.*]] = struct_extract %0, #TestGeneral.funcV
  // CHECK-NEXT: %3 = unchecked_trivial_bit_cast [[FUNCV_REF]] to $S<(Array<Any>) -> Optional<Any>>
  // CHECK: [[DATA_REF:%.*]] = struct_extract %20, #<abstract function>Test.data
  // CHECK-NEXT: [[DATA_COPY:%.*]] = copy_value [[DATA_REF]]
  // CHECK-NEXT: [[DATA_ANY:%.*]] = unchecked_value_cast [[DATA_COPY]] to $V<(Array<Any>) -> Any>
  // CHECK: [[ACCEPTS_ANY:%.*]] = function_ref @$s37sendable_to_any_for_generic_arguments11TestGeneralV023test_function_types_as_E5_argsyyF08accepts_C0L_yyAcDyyF1VL_VyypSayypGcGF : $@convention(thin) (@guaranteed V<(Array<Any>) -> Any>) -> ()
  // CHECK-NEXT: {{.*}} = apply [[ACCEPTS_ANY]]([[DATA_ANY]]) : $@convention(thin) (@guaranteed V<(Array<Any>) -> Any>) -> ()
  func test_function_types_as_generic_args() {
    let _: S<([Any]) -> Any?> = funcV

    struct V<T> {
      let v: T
    }

    struct Test {
      @preconcurrency var data: V<([any Sendable]) -> any Sendable>
    }


    func accepts_any(_: V<([Any]) -> Any>) {}

    let test = Test(data: V(v: { $0.first! }))
    accepts_any(test.data)
  }
}
