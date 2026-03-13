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
  // CHECK-NEXT: {{%.*}} = unchecked_trivial_bit_cast [[FUNCV_REF]] to $S<(Array<Any>) -> Optional<Any>>
  // CHECK: [[DATA_REF:%.*]] = struct_extract %{{.*}}, #<abstract function>Test.data
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

extension Dictionary where Key == String, Value == Any {
  subscript<T>(entry object: T) -> T? {
    get { nil }
    set { }
  }

  var test: Int? {
    get { nil }
    set { }
  }

  mutating func testMutating() {}
}

func test_subscript_computed_property_and_mutating_access(u: User) {
  // CHECK:  [[DICT_GETTER:%.*]] = class_method %0, #User.dict!getter : (User) -> () -> [String : any Sendable], $@convention(method) (@guaranteed User) -> @owned Dictionary<String, any Sendable>
  // CHECK-NEXT: [[DICT:%.*]] = apply [[DICT_GETTER]]({{.*}}) : $@convention(method) (@guaranteed User) -> @owned Dictionary<String, any Sendable>
  // CHECK-NEXT: [[ANY_DICT:%.*]] = unchecked_bitwise_cast [[DICT]] to $Dictionary<String, Any>
  // CHECK-NEXT: [[ANY_DICT_COPY:%.*]] = copy_value [[ANY_DICT]]
  // CHECK-NEXT: [[BORROWED_COPY:%.*]] = begin_borrow [[ANY_DICT_COPY]]
  // CHECK: [[SUBSCRIPT_GETTER:%.*]] = function_ref @$sSD37sendable_to_any_for_generic_argumentsSSRszypRs_rlE5entryqd__Sgqd___tcluig
  // CHECK-NEXT: {{.*}} = apply [[SUBSCRIPT_GETTER]]<String, Any, String>({{.*}}, [[BORROWED_COPY]])
  _ = u.dict[entry: ""]

  // CHECK: [[DICT_GETTER:%.*]] = class_method %0, #User.dict!modify : (User) -> () -> (), $@yield_once @convention(method) (@guaranteed User) -> @yields @inout Dictionary<String, any Sendable>
  // CHECK-NEXT: ([[DICT_ADDR:%.*]], {{.*}}) = begin_apply [[DICT_GETTER]]({{.*}}) : $@yield_once @convention(method) (@guaranteed User) -> @yields @inout Dictionary<String, any Sendable>
  // CHECK-NEXT: [[ANY_DICT:%.*]] = alloc_stack $Dictionary<String, Any>
  // CHECK-NEXT: [[LOADED_DICT:%.*]] = load [copy] [[DICT_ADDR]]
  // CHECK-NEXT: [[ANY_LOADED_DICT:%.*]] = unchecked_bitwise_cast [[LOADED_DICT]] to $Dictionary<String, Any>
  // CHECK-NEXT: [[COPIED_ANY_DICT:%.*]] = copy_value [[ANY_LOADED_DICT]]
  // CHECK-NEXT: store [[COPIED_ANY_DICT]] to [init] [[ANY_DICT]]
  // CHECK: [[SUBSCRIPT_SETTER:%.*]] = function_ref @$sSD37sendable_to_any_for_generic_argumentsSSRszypRs_rlE5entryqd__Sgqd___tcluis
  // CHECK-NEXT: %{{.*}} = apply [[SUBSCRIPT_SETTER]]<String, Any, Int>({{.*}}, [[ANY_DICT]])
  // CHECK-NEXT: [[LOADED_ANY_DICT:%.*]] = load [take] [[ANY_DICT]]
  // CHECK-NEXT: [[SENDABLE_DICT:%.*]] = unchecked_bitwise_cast [[LOADED_ANY_DICT]] to $Dictionary<String, any Sendable>
  // CHECK-NEXT: [[COPIED_SENDABLE_DICT:%.*]] = copy_value [[SENDABLE_DICT]]
  // CHECK-NEXT: assign [[COPIED_SENDABLE_DICT]] to [[DICT_ADDR]]
  u.dict[entry: 42] = 42

  // CHECK: [[DICT_GETTER:%.*]] = class_method %0, #User.dict!getter : (User) -> () -> [String : any Sendable], $@convention(method) (@guaranteed User) -> @owned Dictionary<String, any Sendable>
  // CHECK-NEXT: [[SENDABLE_DICT:%.*]] = apply [[DICT_GETTER]]({{.*}}) : $@convention(method) (@guaranteed User) -> @owned Dictionary<String, any Sendable>
  // CHECK-NEXT: [[DICT_CAST_TO_ANY:%.*]] = unchecked_bitwise_cast [[SENDABLE_DICT]] to $Dictionary<String, Any>
  // CHECK-NEXT: [[ANY_DICT_COPY:%.*]] = copy_value [[DICT_CAST_TO_ANY]]
  // CHECK-NEXT: [[ANY_DICT:%.*]] = begin_borrow [[ANY_DICT_COPY]]
  // CHECK: [[GETTER:%.*]] = function_ref @$sSD37sendable_to_any_for_generic_argumentsSSRszypRs_rlE4testSiSgvg
  // CHECK-NEXT: {{.*}} = apply [[GETTER]]([[ANY_DICT]]) : $@convention(method) (@guaranteed Dictionary<String, Any>) -> Optional<Int>
  _ = u.dict.test

  // CHECK: [[DICT_GETTER:%.*]] = class_method %0, #User.dict!modify : (User) -> () -> (), $@yield_once @convention(method) (@guaranteed User) -> @yields @inout Dictionary<String, any Sendable>
  // CHECK-NEXT: ([[DICT:%.*]], {{.*}}) = begin_apply [[DICT_GETTER]]({{.*}}) : $@yield_once @convention(method) (@guaranteed User) -> @yields @inout Dictionary<String, any Sendable>
  // CHECK-NEXT: [[ANY_DICT:%.*]] = alloc_stack $Dictionary<String, Any>
  // CHECK-NEXT: [[LOADED_DICT:%.*]] = load [copy] [[DICT]]
  // CHECK-NEXT: [[CASTED_DICT:%.*]] = unchecked_bitwise_cast [[LOADED_DICT]] to $Dictionary<String, Any>
  // CHECK-NEXT: [[COPIED_CASTED_DICT:%.*]] = copy_value [[CASTED_DICT]]
  // CHECK-NEXT: store [[COPIED_CASTED_DICT]] to [init] [[ANY_DICT]]
  // CHECK: [[SETTER:%.*]] = function_ref @$sSD37sendable_to_any_for_generic_argumentsSSRszypRs_rlE4testSiSgvs
  // CHECK-NEXT: {{.*}} = apply [[SETTER]]({{.*}}, [[ANY_DICT]]) : $@convention(method) (Optional<Int>, @inout Dictionary<String, Any>) -> ()
  // CHECK-NEXT: [[LOADED_ANY_DICT:%.*]] = load [take] [[ANY_DICT]]
  // CHECK-NEXT: [[SENDABLE_DICT:%.*]] = unchecked_bitwise_cast [[LOADED_ANY_DICT]] to $Dictionary<String, any Sendable>
  // CHECK-NEXT: [[COPIED_SENDABLE_DICT:%.*]] = copy_value [[SENDABLE_DICT]]
  // CHECK-NEXT: assign [[COPIED_SENDABLE_DICT]] to [[DICT]]
  u.dict.test = 42

  // CHECK: [[DICT_GETTER:%.*]] = class_method %0, #User.dict!modify : (User) -> () -> (), $@yield_once @convention(method) (@guaranteed User) -> @yields @inout Dictionary<String, any Sendable>
  // CHECK-NEXT: ([[DICT:%.*]], {{.*}}) = begin_apply [[DICT_GETTER:%.*]](%0) : $@yield_once @convention(method) (@guaranteed User) -> @yields @inout Dictionary<String, any Sendable>
  // CHECK-NEXT: [[ANY_DICT:%.*]] = alloc_stack $Dictionary<String, Any>
  // CHECK-NEXT: [[LOADED_DICT:%.*]] = load [copy] [[DICT]]
  // CHECK-NEXT: [[CASTED_DICT:%.*]] = unchecked_bitwise_cast [[LOADED_DICT]] to $Dictionary<String, Any>
  // CHECK-NEXT: [[COPIED_DICT:%.*]] = copy_value [[CASTED_DICT]]
  // CHECK-NEXT: store [[COPIED_DICT]] to [init] [[ANY_DICT]]
  // CHECK: [[MUTATING_METHOD:%.*]] = function_ref @$sSD37sendable_to_any_for_generic_argumentsSSRszypRs_rlE12testMutatingyyF : $@convention(method) (@inout Dictionary<String, Any>) -> ()
  // CHECK-NEXT: {{%.*}} = apply [[MUTATING_METHOD]]([[ANY_DICT]]) : $@convention(method) (@inout Dictionary<String, Any>) -> ()
  // CHECK-NEXT: [[LOADED_ANY_DICT:%.*]] = load [take] [[ANY_DICT]]
  // CHECK-NEXT: [[SENDABLE_DICT:%.*]] = unchecked_bitwise_cast [[LOADED_ANY_DICT]] to $Dictionary<String, any Sendable>
  // CHECK-NEXT: [[COPIED_SENDABLE_DICT:%.*]] = copy_value [[SENDABLE_DICT]]
  // CHECK-NEXT: assign [[COPIED_SENDABLE_DICT]] to [[DICT]]
  u.dict.testMutating()
}

// CHECK-LABEL: sil hidden [ossa] @$s37sendable_to_any_for_generic_arguments15test_inout_usesyyF
// CHECK: [[SENDABLE_ARR_REF:%.*]] = begin_access [modify] [unknown] %2
// CHECK-NEXT:  [[ANY_ARR:%.*]] = alloc_stack $Array<Any>
// CHECK-NEXT:  [[SENDABLE_ARR:%.*]] = load [copy] [[SENDABLE_ARR_REF]]
// CHECK-NEXT:  [[ANY_ARR_CAST:%.*]] = unchecked_bitwise_cast [[SENDABLE_ARR]] to $Array<Any>
// CHECK-NEXT:  [[ANY_ARR_COPY:%.*]] = copy_value [[ANY_ARR_CAST]]
// CHECK-NEXT:  store [[ANY_ARR_COPY]] to [init] [[ANY_ARR]]
// CHECK: [[INOUT_FUNC:%.*]] = function_ref @$s37sendable_to_any_for_generic_arguments15test_inout_usesyyF0G0L_yySayypGzF : $@convention(thin) (@inout Array<Any>) -> ()
// CHECK-NEXT:  {{.*}} = apply [[INOUT_FUNC]]([[ANY_ARR]]) : $@convention(thin) (@inout Array<Any>) -> ()
// CHECK-NEXT: [[ANY_ARR_VALUE:%.*]] = load [take] [[ANY_ARR]]
// CHECK-NEXT: [[SENDABLE_ARR_VALUE:%.*]] = unchecked_bitwise_cast [[ANY_ARR_VALUE]] to $Array<any Sendable>
// CHECK-NEXT: [[SENDABLE_ARR_VALUE_COPY:%.*]] = copy_value [[SENDABLE_ARR_VALUE]]
// CHECK-NEXT: assign [[SENDABLE_ARR_VALUE_COPY]] to [[SENDABLE_ARR_REF]]
func test_inout_uses() {
  func test(_ arr: inout [Any]) {
  }

  @preconcurrency var arr: [any Sendable] = []
  test(&arr)
}
