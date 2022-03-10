// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

protocol P {
  associatedtype A

  func covariantSelfMethod1() -> Self
  func covariantSelfMethod2() -> Self?
  func covariantSelfMethod3() -> Self.Type
  func covariantSelfMethod4() -> (Self, Self)
  func covariantSelfMethod5() -> Array<Self>
  func covariantSelfMethod6() -> [String : Self]
  func covariantSelfMethod7(_: (Self) -> Void)
  func covariantSelfMethod8(_: (Self...) -> Void)

  func covariantAssocMethod1() -> A
  func covariantAssocMethod2() -> A?
  func covariantAssocMethod3() -> A.Type
  func covariantAssocMethod4() -> (A, A)
  func covariantAssocMethod5() -> Array<A>
  func covariantAssocMethod6() -> [String : A]
  func covariantAssocMethod7(_: (A) -> Void)
  func covariantAssocMethod8(_: (A...) -> Void)

  var covariantSelfProperty1: Self { get }
  var covariantSelfProperty2: Self? { get }
  var covariantSelfProperty3: Self.Type { get }
  var covariantSelfProperty4: (Self, Self) { get }
  var covariantSelfProperty5: Array<Self> { get }
  var covariantSelfProperty6: [String : Self] { get }
  var covariantSelfProperty7: ((Self) -> Void) -> Void { get }
  var covariantSelfProperty8: ((Self...) -> Void) -> Void { get }

  var covariantAssocProperty1: A { get }
  var covariantAssocProperty2: A? { get }
  var covariantAssocProperty3: A.Type { get }
  var covariantAssocProperty4: (A, A) { get }
  var covariantAssocProperty5: Array<A> { get }
  var covariantAssocProperty6: [String : A] { get }
  var covariantAssocProperty7: ((A) -> Void) -> Void { get }
  var covariantAssocProperty8: ((A...) -> Void) -> Void { get }

  subscript(covariantSelfSubscript1 _: Void) -> Self { get }
  subscript(covariantSelfSubscript2 _: Void) -> Self? { get }
  subscript(covariantSelfSubscript3 _: Void) -> Self.Type { get }
  subscript(covariantSelfSubscript4 _: Void) -> (Self, Self) { get }
  subscript(covariantSelfSubscript5 _: Void) -> Array<Self> { get }
  subscript(covariantSelfSubscript6 _: Void) -> [String : Self] { get }
  subscript(covariantSelfSubscript7 _: (Self) -> Void) -> Void { get }
  subscript(covariantSelfSubscript8 _: (Self...) -> Void) -> Void { get }

  subscript(covariantAssocSubscript1 _: Void) -> A { get }
  subscript(covariantAssocSubscript2 _: Void) -> A? { get }
  subscript(covariantAssocSubscript3 _: Void) -> A.Type { get }
  subscript(covariantAssocSubscript4 _: Void) -> (A, A) { get }
  subscript(covariantAssocSubscript5 _: Void) -> Array<A> { get }
  subscript(covariantAssocSubscript6 _: Void) -> [String : A] { get }
  subscript(covariantAssocSubscript7 _: (A) -> Void) -> Void { get }
  subscript(covariantAssocSubscript8 _: (A...) -> Void) -> Void { get }
}

// -----------------------------------------------------------------------------
// Covariant 'Self' erasure
// -----------------------------------------------------------------------------

// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype24testCovariantSelfMethod11pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $P, let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantSelfMethod1 : <Self where Self : P> (Self) -> () -> Self
// CHECK: [[DEST:%[0-9]+]] = init_existential_addr [[LET]] : $*P, $@opened([[OPENED_ID]]) P
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[DEST]], [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
func testCovariantSelfMethod1(p: any P) {
  let x = p.covariantSelfMethod1()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype24testCovariantSelfMethod21pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $Optional<P>, let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantSelfMethod2 : <Self where Self : P> (Self) -> () -> Self?
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>(%{{[0-9]+}}, [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out Optional<τ_0_0>
// CHECK: init_existential_addr %{{[0-9]+}} : $*P, $@opened([[OPENED_ID]]) P
func testCovariantSelfMethod2(p: any P) {
  let x = p.covariantSelfMethod2()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype24testCovariantSelfMethod31pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantSelfMethod3 : <Self where Self : P> (Self) -> () -> Self.Type
// CHECK: [[META:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @thick τ_0_0.Type
// CHECK: [[EXIST_META:%[0-9]+]] = init_existential_metatype [[META]] : $@thick (@opened([[OPENED_ID]]) P).Type, $@thick P.Type
// CHECK: debug_value [[EXIST_META]] : $@thick P.Type, let, name "x"
func testCovariantSelfMethod3(p: any P) {
  let x = p.covariantSelfMethod3()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype24testCovariantSelfMethod41pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $(P, P), let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantSelfMethod4 : <Self where Self : P> (Self) -> () -> (Self, Self)
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[SRC_0:%[0-9]+]], [[SRC_1:%[0-9]+]], [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> (@out τ_0_0, @out τ_0_0)
// CHECK: init_existential_addr %{{[0-9]+}} : $*P, $@opened([[OPENED_ID]]) P
// CHECK: init_existential_addr %{{[0-9]+}} : $*P, $@opened([[OPENED_ID]]) P
func testCovariantSelfMethod4(p: any P) {
  let x = p.covariantSelfMethod4()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype24testCovariantSelfMethod51pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantSelfMethod5 : <Self where Self : P> (Self) -> () -> Array<Self>
// CHECK: [[ARRAY:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned Array<τ_0_0>
// CHECK: [[ARRAY_UPCAST_FN:%[0-9]+]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: [[RESULT:%[0-9]+]] = apply [[ARRAY_UPCAST_FN]]<@opened([[OPENED_ID]]) P, P>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: debug_value %{{[0-9]+}} : $Array<P>, let, name "x"
func testCovariantSelfMethod5(p: any P) {
  let x = p.covariantSelfMethod5()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype24testCovariantSelfMethod61pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantSelfMethod6 : <Self where Self : P> (Self) -> () -> [String : Self]
// CHECK: [[DICT:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned Dictionary<String, τ_0_0>
// CHECK: [[DICT_UPCAST_FN:%[0-9]+]] = function_ref @$ss17_dictionaryUpCastySDyq0_q1_GSDyxq_GSHRzSHR0_r2_lF : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: [[RESULT:%[0-9]+]] = apply [[DICT_UPCAST_FN]]<String, @opened([[OPENED_ID]]) P, String, P>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: debug_value %{{[0-9]+}} : $Dictionary<String, P>, let, name "x"
func testCovariantSelfMethod6(p: any P) {
  let x = p.covariantSelfMethod6()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype24testCovariantSelfMethod71pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[THUNK:%[0-9]+]] = function_ref @$s42existential_member_accesses_self_assoctype1P_pIgn_xIegn_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0, @noescape @callee_guaranteed (@in_guaranteed P) -> ()) -> ()
// CHECK: [[STEP1:%[0-9]+]] = partial_apply [callee_guaranteed] [[THUNK]]<@opened([[OPENED_ID]]) P>
// CHECK: [[STEP2:%[0-9]+]] = convert_function [[STEP1]] : $@callee_guaranteed (@in_guaranteed @opened([[OPENED_ID]]) P) -> () to $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <@opened([[OPENED_ID]]) P>
// CHECK: [[STEP3:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[STEP2]]
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantSelfMethod7 : <Self where Self : P> (Self) -> ((Self) -> ()) -> ()
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[STEP3]], [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0>, @in_guaranteed τ_0_0) -> ()
func testCovariantSelfMethod7(p: any P) {
  p.covariantSelfMethod7 { _ in }
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype24testCovariantSelfMethod81pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*[[OPENED_TY:@opened\("[0-9A-F-]+"\) P]]
// CHECK: [[THUNK:%[0-9]+]] = function_ref @[[SELF_ARRAY_THUNK_NAME:\$[0-9a-zA-Z_]+]] : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@guaranteed Array<τ_0_0>, @noescape @callee_guaranteed (@guaranteed Array<P>) -> ()) -> ()
// CHECK: [[STEP1:%[0-9]+]] = partial_apply [callee_guaranteed] [[THUNK]]<[[OPENED_TY]]>
// CHECK: [[STEP2:%[0-9]+]] = convert_function [[STEP1]] : $@callee_guaranteed (@guaranteed Array<[[OPENED_TY]]>) -> () to $@callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <[[OPENED_TY]], [[OPENED_TY]]>
// CHECK: [[STEP3:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[STEP2]]
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $[[OPENED_TY]], #P.covariantSelfMethod8 : <Self where Self : P> (Self) -> ((Self...) -> ()) -> ()
// CHECK: apply [[WITNESS]]<[[OPENED_TY]]>([[STEP3]], [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <τ_0_0, τ_0_0>, @in_guaranteed τ_0_0) -> ()

// CHECK: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @[[SELF_ARRAY_THUNK_NAME]]
// CHECK: [[ARRAY_UPCAST:%[0-9]+]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF
// CHECK: apply [[ARRAY_UPCAST]]<τ_0_0, P>
// CHECK: } // end sil function '[[SELF_ARRAY_THUNK_NAME]]'
func testCovariantSelfMethod8(p: any P) {
  p.covariantSelfMethod8 { _ in }
}

// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype26testCovariantSelfProperty11pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $P, let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// FIXME: What's this copy for?
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantSelfProperty1!getter : <Self where Self : P> (Self) -> () -> Self
// CHECK: [[DEST:%[0-9]+]] = init_existential_addr [[LET]] : $*P, $@opened([[OPENED_ID]]) P
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[DEST]], [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
func testCovariantSelfProperty1(p: any P) {
  let x = p.covariantSelfProperty1
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype26testCovariantSelfProperty21pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $Optional<P>, let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantSelfProperty2!getter : <Self where Self : P> (Self) -> () -> Self?
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPTIONAL:%[0-9]+]], [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out Optional<τ_0_0>
// CHECK: init_existential_addr %{{[0-9]+}} : $*P, $@opened([[OPENED_ID]]) P
func testCovariantSelfProperty2(p: any P) {
  let x = p.covariantSelfProperty2
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype26testCovariantSelfProperty31pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantSelfProperty3!getter : <Self where Self : P> (Self) -> () -> Self.Type
// CHECK: [[META:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @thick τ_0_0.Type
// CHECK: [[EXIST_META:%[0-9]+]] = init_existential_metatype [[META]] : $@thick (@opened([[OPENED_ID]]) P).Type, $@thick P.Type
// CHECK: debug_value [[EXIST_META]] : $@thick P.Type, let, name "x"
func testCovariantSelfProperty3(p: any P) {
  let x = p.covariantSelfProperty3
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype26testCovariantSelfProperty41pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $(P, P), let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantSelfProperty4!getter : <Self where Self : P> (Self) -> () -> (Self, Self)
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[SRC_0:%[0-9]+]], [[SRC_1:%[0-9]+]], [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> (@out τ_0_0, @out τ_0_0)
// CHECK: [[DEST_0_INITED:%[0-9]+]] = init_existential_addr %{{[0-9]+}} : $*P, $@opened([[OPENED_ID]]) P
// CHECK: [[DEST_1_INITED:%[0-9]+]] = init_existential_addr %{{[0-9]+}} : $*P, $@opened([[OPENED_ID]]) P
func testCovariantSelfProperty4(p: any P) {
  let x = p.covariantSelfProperty4
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype26testCovariantSelfProperty51pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantSelfProperty5!getter : <Self where Self : P> (Self) -> () -> Array<Self>
// CHECK: [[ARRAY:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned Array<τ_0_0>
// CHECK: [[ARRAY_UPCAST_FN:%[0-9]+]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: [[RESULT:%[0-9]+]] = apply [[ARRAY_UPCAST_FN]]<@opened([[OPENED_ID]]) P, P>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: debug_value %{{[0-9]+}} : $Array<P>, let, name "x"
func testCovariantSelfProperty5(p: any P) {
  let x = p.covariantSelfProperty5
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype26testCovariantSelfProperty61pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantSelfProperty6!getter : <Self where Self : P> (Self) -> () -> [String : Self]
// CHECK: [[DICT:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned Dictionary<String, τ_0_0>
// CHECK: [[DICT_UPCAST_FN:%[0-9]+]] = function_ref @$ss17_dictionaryUpCastySDyq0_q1_GSDyxq_GSHRzSHR0_r2_lF : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: [[RESULT:%[0-9]+]] = apply [[DICT_UPCAST_FN]]<String, @opened([[OPENED_ID]]) P, String, P>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: debug_value %{{[0-9]+}} : $Dictionary<String, P>, let, name "x"
func testCovariantSelfProperty6(p: any P) {
  let x = p.covariantSelfProperty6
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype26testCovariantSelfProperty71pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P,  #P.covariantSelfProperty7!getter : <Self where Self : P> (Self) -> () -> ((Self) -> ()) -> ()
// CHECK: [[RESULT_FN:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned @callee_guaranteed @substituted <τ_0_0> (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0>) -> () for <τ_0_0>
// CHECK: [[STEP1:%[0-9]+]] = convert_function [[RESULT_FN]] : $@callee_guaranteed @substituted <τ_0_0> (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0>) -> () for <@opened([[OPENED_ID]]) P> to $@callee_guaranteed (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <@opened([[OPENED_ID]]) P>) -> ()
// CHECK: [[THUNK:%[0-9]+]] = function_ref @$sxlyxIsgn_Iegy_42existential_member_accesses_self_assoctype1P_pIgn_Iegy_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed (@in_guaranteed P) -> (), @guaranteed @callee_guaranteed (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0>) -> ()) -> ()
// CHECK: partial_apply [callee_guaranteed] [[THUNK]]<@opened([[OPENED_ID]]) P>([[STEP1]]) : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed (@in_guaranteed P) -> (), @guaranteed @callee_guaranteed (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0>) -> ()) -> ()
// CHECK: debug_value %{{[0-9]+}} : $@callee_guaranteed (@noescape @callee_guaranteed (@in_guaranteed P) -> ()) -> (), let, name "x"
func testCovariantSelfProperty7(p: any P) {
  let x = p.covariantSelfProperty7
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype26testCovariantSelfProperty81pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*[[OPENED_TY:@opened\("[0-9A-F-]+"\) P]]
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $[[OPENED_TY]]
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*[[OPENED_TY]]
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $[[OPENED_TY]],  #P.covariantSelfProperty8!getter : <Self where Self : P> (Self) -> () -> ((Self...) -> ()) -> ()
// CHECK: [[RESULT_FN:%[0-9]+]] = apply [[WITNESS]]<[[OPENED_TY]]>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <τ_0_0, τ_0_0>) -> () for <τ_0_0, τ_0_0>
// CHECK: [[STEP1:%[0-9]+]] = convert_function [[RESULT_FN]] : $@callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <τ_0_0, τ_0_0>) -> () for <[[OPENED_TY]], [[OPENED_TY]]> to $@callee_guaranteed (@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <[[OPENED_TY]], [[OPENED_TY]]>) -> ()
// CHECK: [[THUNK:%[0-9]+]] = function_ref @[[THUNK_NAME:\$[0-9a-zA-Z_]+]] : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed (@guaranteed Array<P>) -> (), @guaranteed @callee_guaranteed (@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <τ_0_0, τ_0_0>) -> ()) -> ()
// CHECK: partial_apply [callee_guaranteed] [[THUNK]]<[[OPENED_TY]]>([[STEP1]])
// CHECK: debug_value %{{[0-9]+}} : $@callee_guaranteed (@noescape @callee_guaranteed (@guaranteed Array<P>) -> ()) -> (), let, name "x"
// CHECK: } // end sil function '$s42existential_member_accesses_self_assoctype26testCovariantSelfProperty81pyAA1P_p_tF'

// CHECK: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @[[THUNK_NAME]]
// CHECK: function_ref @[[SELF_ARRAY_THUNK_NAME]]
// CHECK: } // end sil function '[[THUNK_NAME]]'
func testCovariantSelfProperty8(p: any P) {
  let x = p.covariantSelfProperty8
}

// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantSelfSubscript11pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $P, let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.subscript!getter : <Self where Self : P> (Self) -> (()) -> Self
// CHECK: [[DEST:%[0-9]+]] = init_existential_addr [[LET]] : $*P, $@opened([[OPENED_ID]]) P
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[DEST]], [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
func testCovariantSelfSubscript1(p: any P) {
  let x = p[covariantSelfSubscript1: ()]
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantSelfSubscript21pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $Optional<P>, let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.subscript!getter : <Self where Self : P> (Self) -> (()) -> Self?
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPTIONAL:%[0-9]+]], [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out Optional<τ_0_0>
// CHECK: init_existential_addr %{{[0-9]+}} : $*P, $@opened([[OPENED_ID]]) P
func testCovariantSelfSubscript2(p: any P) {
  let x = p[covariantSelfSubscript2: ()]
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantSelfSubscript31pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.subscript!getter : <Self where Self : P> (Self) -> (()) -> Self.Type
// CHECK: [[META:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @thick τ_0_0.Type
// CHECK: [[EXIST_META:%[0-9]+]] = init_existential_metatype [[META]] : $@thick (@opened([[OPENED_ID]]) P).Type, $@thick P.Type
// CHECK: debug_value [[EXIST_META]] : $@thick P.Type, let, name "x"
func testCovariantSelfSubscript3(p: any P) {
  let x = p[covariantSelfSubscript3: ()]
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantSelfSubscript41pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $(P, P), let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.subscript!getter : <Self where Self : P> (Self) -> (()) -> (Self, Self)
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[SRC_0:%[0-9]+]], [[SRC_1:%[0-9]+]], [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> (@out τ_0_0, @out τ_0_0)
// CHECK: init_existential_addr %{{[0-9]+}} : $*P, $@opened([[OPENED_ID]]) P
// CHECK: init_existential_addr %{{[0-9]+}} : $*P, $@opened([[OPENED_ID]]) P
func testCovariantSelfSubscript4(p: any P) {
  let x = p[covariantSelfSubscript4: ()]
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantSelfSubscript51pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.subscript!getter : <Self where Self : P> (Self) -> (()) -> Array<Self>
// CHECK: [[ARRAY:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned Array<τ_0_0>
// CHECK: [[ARRAY_UPCAST_FN:%[0-9]+]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: [[RESULT:%[0-9]+]] = apply [[ARRAY_UPCAST_FN]]<@opened([[OPENED_ID]]) P, P>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: debug_value %{{[0-9]+}} : $Array<P>, let, name "x"
func testCovariantSelfSubscript5(p: any P) {
  let x = p[covariantSelfSubscript5: ()]
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantSelfSubscript61pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.subscript!getter : <Self where Self : P> (Self) -> (()) -> [String : Self]
// CHECK: [[DICT:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned Dictionary<String, τ_0_0>
// CHECK: [[DICT_UPCAST_FN:%[0-9]+]] = function_ref @$ss17_dictionaryUpCastySDyq0_q1_GSDyxq_GSHRzSHR0_r2_lF : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: [[RESULT:%[0-9]+]] = apply [[DICT_UPCAST_FN]]<String, @opened([[OPENED_ID]]) P, String, P>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: debug_value %{{[0-9]+}} : $Dictionary<String, P>, let, name "x"
func testCovariantSelfSubscript6(p: any P) {
  let x = p[covariantSelfSubscript6: ()]
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantSelfSubscript71pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[THUNK:%[0-9]+]] = function_ref @$s42existential_member_accesses_self_assoctype1P_pIgn_xIegn_AaBRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0, @noescape @callee_guaranteed (@in_guaranteed P) -> ()) -> ()
// CHECK: [[STEP1:%[0-9]+]] = partial_apply [callee_guaranteed] [[THUNK]]<@opened([[OPENED_ID]]) P>
// CHECK: [[STEP2:%[0-9]+]] = convert_function [[STEP1]] : $@callee_guaranteed (@in_guaranteed @opened([[OPENED_ID]]) P) -> () to $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <@opened([[OPENED_ID]]) P>
// CHECK: [[STEP3:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[STEP2]]
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.subscript!getter : <Self where Self : P> (Self) -> ((Self) -> ()) -> ()
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[STEP3]], [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0>, @in_guaranteed τ_0_0) -> ()
func testCovariantSelfSubscript7(p: any P) {
  _ = p[covariantSelfSubscript7: { _ in }]
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantSelfSubscript81pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*[[OPENED_TY:@opened\("[0-9A-F-]+"\) P]]
// CHECK: [[THUNK:%[0-9]+]] = function_ref @[[SELF_ARRAY_THUNK_NAME]] : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@guaranteed Array<τ_0_0>, @noescape @callee_guaranteed (@guaranteed Array<P>) -> ()) -> ()
// CHECK: [[STEP1:%[0-9]+]] = partial_apply [callee_guaranteed] [[THUNK]]<[[OPENED_TY]]>
// CHECK: [[STEP2:%[0-9]+]] = convert_function [[STEP1]] : $@callee_guaranteed (@guaranteed Array<[[OPENED_TY]]>) -> () to $@callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <[[OPENED_TY]], [[OPENED_TY]]>
// CHECK: [[STEP3:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[STEP2]]
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $[[OPENED_TY]]
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*[[OPENED_TY]]
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $[[OPENED_TY]], #P.subscript!getter : <Self where Self : P> (Self) -> ((Self...) -> ()) -> ()
// CHECK: apply [[WITNESS]]<[[OPENED_TY]]>([[STEP3]], [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <τ_0_0, τ_0_0>, @in_guaranteed τ_0_0) -> ()
func testCovariantSelfSubscript8(p: any P) {
  _ = p[covariantSelfSubscript8: { _ in }]
}

// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype25testCovariantAssocMethod11pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $Any, let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantAssocMethod1 : <Self where Self : P> (Self) -> () -> Self.A
// CHECK: [[DEST:%[0-9]+]] = init_existential_addr [[LET]] : $*Any, $(@opened([[OPENED_ID]]) P).A
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[DEST]], [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.A
func testCovariantAssocMethod1(p: any P) {
  let x = p.covariantAssocMethod1()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype25testCovariantAssocMethod21pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $Optional<Any>, let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantAssocMethod2 : <Self where Self : P> (Self) -> () -> Self.A?
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>(%{{[0-9]+}}, [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out Optional<τ_0_0.A>
// CHECK: init_existential_addr %{{[0-9]+}} : $*Any, $(@opened([[OPENED_ID]]) P).A
func testCovariantAssocMethod2(p: any P) {
  let x = p.covariantAssocMethod2()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype25testCovariantAssocMethod31pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantAssocMethod3 : <Self where Self : P> (Self) -> () -> Self.A.Type
// CHECK: [[META:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @thick τ_0_0.A.Type
// CHECK: [[EXIST_META:%[0-9]+]] = init_existential_metatype [[META]] : $@thick ((@opened([[OPENED_ID]]) P).A).Type, $@thick Any.Type
// CHECK: debug_value [[EXIST_META]] : $@thick Any.Type, let, name "x"
func testCovariantAssocMethod3(p: any P) {
  let x = p.covariantAssocMethod3()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype25testCovariantAssocMethod41pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $(Any, Any), let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantAssocMethod4 : <Self where Self : P> (Self) -> () -> (Self.A, Self.A)
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>(%{{[0-9]+}}, %{{[0-9]+}}, [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> (@out τ_0_0.A, @out τ_0_0.A)
// CHECK: init_existential_addr %{{[0-9]+}} : $*Any, $(@opened([[OPENED_ID]]) P).A
// CHECK: init_existential_addr %{{[0-9]+}} : $*Any, $(@opened([[OPENED_ID]]) P).A
func testCovariantAssocMethod4(p: any P) {
  let x = p.covariantAssocMethod4()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype25testCovariantAssocMethod51pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantAssocMethod5 : <Self where Self : P> (Self) -> () -> Array<Self.A>
// CHECK: [[ARRAY:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned Array<τ_0_0.A>
// CHECK: [[ARRAY_UPCAST_FN:%[0-9]+]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: [[RESULT:%[0-9]+]] = apply [[ARRAY_UPCAST_FN]]<(@opened([[OPENED_ID]]) P).A, Any>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: debug_value %{{[0-9]+}} : $Array<Any>, let, name "x"
func testCovariantAssocMethod5(p: any P) {
  let x = p.covariantAssocMethod5()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype25testCovariantAssocMethod61pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantAssocMethod6 : <Self where Self : P> (Self) -> () -> [String : Self.A]
// CHECK: [[DICT:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned Dictionary<String, τ_0_0.A>
// CHECK: [[DICT_UPCAST_FN:%[0-9]+]] = function_ref @$ss17_dictionaryUpCastySDyq0_q1_GSDyxq_GSHRzSHR0_r2_lF : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: [[RESULT:%[0-9]+]] = apply [[DICT_UPCAST_FN]]<String, (@opened([[OPENED_ID]]) P).A, String, Any>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: debug_value %{{[0-9]+}} : $Dictionary<String, Any>, let, name "x"
func testCovariantAssocMethod6(p: any P) {
  let x = p.covariantAssocMethod6()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype25testCovariantAssocMethod71pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: function_ref thunk for @callee_guaranteed (@in_guaranteed Any) -> ()
// CHECK-NEXT: function_ref @$sypIgn_1A42existential_member_accesses_self_assoctype1PPQzIegn_AbCRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0.A, @noescape @callee_guaranteed (@in_guaranteed Any) -> ()) -> ()
// CHECK: [[STEP1:%[0-9]+]] = partial_apply [callee_guaranteed] [[THUNK]]<@opened([[OPENED_ID]]) P>
// CHECK: [[STEP2:%[0-9]+]] = convert_function [[STEP1]] : $@callee_guaranteed (@in_guaranteed (@opened([[OPENED_ID]]) P).A) -> () to $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <(@opened([[OPENED_ID]]) P).A>
// CHECK: [[STEP3:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[STEP2]]
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantAssocMethod7 : <Self where Self : P> (Self) -> ((Self.A) -> ()) -> ()
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[STEP3]], [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0.A>, @in_guaranteed τ_0_0) -> ()
func testCovariantAssocMethod7(p: any P) {
  p.covariantAssocMethod7 { _ in }
}
// CHECK-LABEL: sil hidden [ossa]  @$s42existential_member_accesses_self_assoctype25testCovariantAssocMethod81pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*[[OPENED_TY:@opened\("[0-9A-F-]+"\) P]]
// CHECK: [[THUNK:%[0-9]+]] = function_ref @[[ASSOCTYPE_ARRAY_THUNK_NAME:\$[0-9a-zA-Z_]+]] : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@guaranteed Array<τ_0_0.A>, @noescape @callee_guaranteed (@guaranteed Array<Any>) -> ()) -> ()
// CHECK: [[STEP1:%[0-9]+]] = partial_apply [callee_guaranteed] [[THUNK]]<[[OPENED_TY]]>
// CHECK: [[STEP2:%[0-9]+]] = convert_function [[STEP1]] : $@callee_guaranteed (@guaranteed Array<([[OPENED_TY]]).A>) -> () to $@callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <([[OPENED_TY]]).A, ([[OPENED_TY]]).A>
// CHECK: [[STEP3:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[STEP2]]
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $[[OPENED_TY]], #P.covariantAssocMethod8 : <Self where Self : P> (Self) -> ((Self.A...) -> ()) -> ()
// CHECK: apply [[WITNESS]]<[[OPENED_TY]]>([[STEP3]], [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <τ_0_0.A, τ_0_0.A>, @in_guaranteed τ_0_0) -> ()

// CHECK: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @[[ASSOCTYPE_ARRAY_THUNK_NAME]]
// CHECK: [[ARRAY_UPCAST:%[0-9]+]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF
// CHECK: apply [[ARRAY_UPCAST]]<τ_0_0.A, Any>
// CHECK: } // end sil function '[[ASSOCTYPE_ARRAY_THUNK_NAME]]'
func testCovariantAssocMethod8(p: any P) {
  p.covariantAssocMethod8 { _ in }
}

// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantAssocProperty11pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $Any, let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantAssocProperty1!getter : <Self where Self : P> (Self) -> () -> Self.A
// CHECK: [[DEST:%[0-9]+]] = init_existential_addr [[LET]] : $*Any, $(@opened([[OPENED_ID]]) P).A
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[DEST]], [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.A
func testCovariantAssocProperty1(p: any P) {
  let x = p.covariantAssocProperty1
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantAssocProperty21pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $Optional<Any>, let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantAssocProperty2!getter : <Self where Self : P> (Self) -> () -> Self.A?
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>(%{{[0-9]+}}, [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out Optional<τ_0_0.A>
// CHECK: init_existential_addr %{{[0-9]+}} : $*Any, $(@opened([[OPENED_ID]]) P).A
func testCovariantAssocProperty2(p: any P) {
  let x = p.covariantAssocProperty2
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantAssocProperty31pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantAssocProperty3!getter : <Self where Self : P> (Self) -> () -> Self.A.Type
// CHECK: [[META:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @thick τ_0_0.A.Type
// CHECK: [[EXIST_META:%[0-9]+]] = init_existential_metatype [[META]] : $@thick ((@opened([[OPENED_ID]]) P).A).Type, $@thick Any.Type
// CHECK: debug_value [[EXIST_META]] : $@thick Any.Type, let, name "x"
func testCovariantAssocProperty3(p: any P) {
  let x = p.covariantAssocProperty3
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantAssocProperty41pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $(Any, Any), let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantAssocProperty4!getter : <Self where Self : P> (Self) -> () -> (Self.A, Self.A)
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>(%{{[0-9]+}}, %{{[0-9]+}}, [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> (@out τ_0_0.A, @out τ_0_0.A)
// CHECK: init_existential_addr %{{[0-9]+}} : $*Any, $(@opened([[OPENED_ID]]) P).A
// CHECK: init_existential_addr %{{[0-9]+}} : $*Any, $(@opened([[OPENED_ID]]) P).A
func testCovariantAssocProperty4(p: any P) {
  let x = p.covariantAssocProperty4
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantAssocProperty51pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantAssocProperty5!getter : <Self where Self : P> (Self) -> () -> Array<Self.A>
// CHECK: [[ARRAY:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned Array<τ_0_0.A>
// CHECK: [[ARRAY_UPCAST_FN:%[0-9]+]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: [[RESULT:%[0-9]+]] = apply [[ARRAY_UPCAST_FN]]<(@opened([[OPENED_ID]]) P).A, Any>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: debug_value %{{[0-9]+}} : $Array<Any>, let, name "x"
func testCovariantAssocProperty5(p: any P) {
  let x = p.covariantAssocProperty5
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantAssocProperty61pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.covariantAssocProperty6!getter : <Self where Self : P> (Self) -> () -> [String : Self.A]
// CHECK: [[DICT:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned Dictionary<String, τ_0_0.A>
// CHECK: [[DICT_UPCAST_FN:%[0-9]+]] = function_ref @$ss17_dictionaryUpCastySDyq0_q1_GSDyxq_GSHRzSHR0_r2_lF : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: [[RESULT:%[0-9]+]] = apply [[DICT_UPCAST_FN]]<String, (@opened([[OPENED_ID]]) P).A, String, Any>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: debug_value %{{[0-9]+}} : $Dictionary<String, Any>, let, name "x"
func testCovariantAssocProperty6(p: any P) {
  let x = p.covariantAssocProperty6
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantAssocProperty71pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P,  #P.covariantAssocProperty7!getter : <Self where Self : P> (Self) -> () -> ((Self.A) -> ()) -> ()
// CHECK: [[RESULT_FN:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned @callee_guaranteed @substituted <τ_0_0> (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0>) -> () for <τ_0_0.A>
// CHECK: [[STEP1:%[0-9]+]] = convert_function [[RESULT_FN]] : $@callee_guaranteed @substituted <τ_0_0> (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0>) -> () for <(@opened([[OPENED_ID]]) P).A> to $@callee_guaranteed (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <(@opened([[OPENED_ID]]) P).A>) -> ()
// CHECK: [[THUNK:%[0-9]+]] = function_ref @$sxly1A42existential_member_accesses_self_assoctype1PPQzIsgn_Iegy_ypIgn_Iegy_AbCRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed (@in_guaranteed Any) -> (), @guaranteed @callee_guaranteed (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0.A>) -> ()) -> ()
// CHECK: partial_apply [callee_guaranteed] [[THUNK]]<@opened([[OPENED_ID]]) P>([[STEP1]]) : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed (@in_guaranteed Any) -> (), @guaranteed @callee_guaranteed (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0.A>) -> ()) -> ()
// CHECK: debug_value %{{[0-9]+}} : $@callee_guaranteed (@noescape @callee_guaranteed (@in_guaranteed Any) -> ()) -> (), let, name "x"
func testCovariantAssocProperty7(p: any P) {
  let x = p.covariantAssocProperty7
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype27testCovariantAssocProperty81pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*[[OPENED_TY:@opened\("[0-9A-F-]+"\) P]]
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $[[OPENED_TY]]
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*[[OPENED_TY]]
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $[[OPENED_TY]],  #P.covariantAssocProperty8!getter : <Self where Self : P> (Self) -> () -> ((Self.A...) -> ()) -> ()
// CHECK: [[RESULT_FN:%[0-9]+]] = apply [[WITNESS]]<[[OPENED_TY]]>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <τ_0_0, τ_0_0>) -> () for <τ_0_0.A, τ_0_0.A>
// CHECK: [[STEP1:%[0-9]+]] = convert_function [[RESULT_FN]] : $@callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <τ_0_0, τ_0_0>) -> () for <([[OPENED_TY]]).A, ([[OPENED_TY]]).A> to $@callee_guaranteed (@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <([[OPENED_TY]]).A, ([[OPENED_TY]]).A>) -> ()
// CHECK: [[THUNK:%[0-9]+]] = function_ref @[[THUNK_NAME:\$[0-9a-zA-Z_]+]] : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed (@guaranteed Array<Any>) -> (), @guaranteed @callee_guaranteed (@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <τ_0_0.A, τ_0_0.A>) -> ()) -> ()
// CHECK: partial_apply [callee_guaranteed] [[THUNK]]<[[OPENED_TY]]>([[STEP1]])
// CHECK: debug_value %{{[0-9]+}} : $@callee_guaranteed (@noescape @callee_guaranteed (@guaranteed Array<Any>) -> ()) -> (), let, name "x"
// CHECK: } // end sil function '$s42existential_member_accesses_self_assoctype27testCovariantAssocProperty81pyAA1P_p_tF'

// CHECK: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @[[THUNK_NAME]]
// CHECK: function_ref @[[ASSOCTYPE_ARRAY_THUNK_NAME]]
// CHECK: } // end sil function '[[THUNK_NAME]]'
func testCovariantAssocProperty8(p: any P) {
  let x = p.covariantAssocProperty8
}

// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype28testCovariantAssocSubscript11pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $Any, let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.subscript!getter : <Self where Self : P> (Self) -> (()) -> Self.A
// CHECK: [[DEST:%[0-9]+]] = init_existential_addr [[LET]] : $*Any, $(@opened([[OPENED_ID]]) P).A
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[DEST]], [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.A
func testCovariantAssocSubscript1(p: any P) {
  let x = p[covariantAssocSubscript1: ()]
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype28testCovariantAssocSubscript21pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $Optional<Any>, let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.subscript!getter : <Self where Self : P> (Self) -> (()) -> Self.A?
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>(%{{[0-9]+}}, [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out Optional<τ_0_0.A>
// CHECK: init_existential_addr %{{[0-9]+}} : $*Any, $(@opened([[OPENED_ID]]) P).A
func testCovariantAssocSubscript2(p: any P) {
  let x = p[covariantAssocSubscript2: ()]
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype28testCovariantAssocSubscript31pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.subscript!getter : <Self where Self : P> (Self) -> (()) -> Self.A.Type
// CHECK: [[META:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @thick τ_0_0.A.Type
// CHECK: [[EXIST_META:%[0-9]+]] = init_existential_metatype [[META]] : $@thick ((@opened([[OPENED_ID]]) P).A).Type, $@thick Any.Type
// CHECK: debug_value [[EXIST_META]] : $@thick Any.Type, let, name "x"
func testCovariantAssocSubscript3(p: any P) {
  let x = p[covariantAssocSubscript3: ()]
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype28testCovariantAssocSubscript41pyAA1P_p_tF
// CHECK: [[LET:%[0-9]+]] = alloc_stack [lexical] $(Any, Any), let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.subscript!getter : <Self where Self : P> (Self) -> (()) -> (Self.A, Self.A)
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>(%{{[0-9]+}}, %{{[0-9]+}}, [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> (@out τ_0_0.A, @out τ_0_0.A)
// CHECK: init_existential_addr %{{[0-9]+}} : $*Any, $(@opened([[OPENED_ID]]) P).A
// CHECK: init_existential_addr %{{[0-9]+}} : $*Any, $(@opened([[OPENED_ID]]) P).A
func testCovariantAssocSubscript4(p: any P) {
  let x = p[covariantAssocSubscript4: ()]
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype28testCovariantAssocSubscript51pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.subscript!getter : <Self where Self : P> (Self) -> (()) -> Array<Self.A>
// CHECK: [[ARRAY:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned Array<τ_0_0.A>
// CHECK: [[ARRAY_UPCAST_FN:%[0-9]+]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: [[RESULT:%[0-9]+]] = apply [[ARRAY_UPCAST_FN]]<(@opened([[OPENED_ID]]) P).A, Any>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: debug_value %{{[0-9]+}} : $Array<Any>, let, name "x"
func testCovariantAssocSubscript5(p: any P) {
  let x = p[covariantAssocSubscript5: ()]
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype28testCovariantAssocSubscript61pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.subscript!getter : <Self where Self : P> (Self) -> (()) -> [String : Self.A]
// CHECK: [[DICT:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned Dictionary<String, τ_0_0.A>
// CHECK: [[DICT_UPCAST_FN:%[0-9]+]] = function_ref @$ss17_dictionaryUpCastySDyq0_q1_GSDyxq_GSHRzSHR0_r2_lF : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: [[RESULT:%[0-9]+]] = apply [[DICT_UPCAST_FN]]<String, (@opened([[OPENED_ID]]) P).A, String, Any>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: debug_value %{{[0-9]+}} : $Dictionary<String, Any>, let, name "x"
func testCovariantAssocSubscript6(p: any P) {
  let x = p[covariantAssocSubscript6: ()]
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype28testCovariantAssocSubscript71pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P
// CHECK: [[THUNK:%[0-9]+]] = function_ref @$sypIgn_1A42existential_member_accesses_self_assoctype1PPQzIegn_AbCRzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0.A, @noescape @callee_guaranteed (@in_guaranteed Any) -> ()) -> ()
// CHECK: [[STEP1:%[0-9]+]] = partial_apply [callee_guaranteed] [[THUNK]]<@opened([[OPENED_ID]]) P>
// CHECK: [[STEP2:%[0-9]+]] = convert_function [[STEP1]] : $@callee_guaranteed (@in_guaranteed (@opened([[OPENED_ID]]) P).A) -> () to $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <(@opened([[OPENED_ID]]) P).A>
// CHECK: [[STEP3:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[STEP2]]
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $@opened([[OPENED_ID]]) P
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*@opened([[OPENED_ID]]) P
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P, #P.subscript!getter : <Self where Self : P> (Self) -> ((Self.A) -> ()) -> ()
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P>([[STEP3]], [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0.A>, @in_guaranteed τ_0_0) -> ()
func testCovariantAssocSubscript7(p: any P) {
  _ = p[covariantAssocSubscript7: { _ in }]
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype28testCovariantAssocSubscript81pyAA1P_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P to $*[[OPENED_TY:@opened\("[0-9A-F-]+"\) P]]
// CHECK: [[THUNK:%[0-9]+]] = function_ref @[[ASSOCTYPE_ARRAY_THUNK_NAME]] : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@guaranteed Array<τ_0_0.A>, @noescape @callee_guaranteed (@guaranteed Array<Any>) -> ()) -> ()
// CHECK: [[STEP1:%[0-9]+]] = partial_apply [callee_guaranteed] [[THUNK]]<[[OPENED_TY]]>
// CHECK: [[STEP2:%[0-9]+]] = convert_function [[STEP1]] : $@callee_guaranteed (@guaranteed Array<([[OPENED_TY]]).A>) -> () to $@callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <([[OPENED_TY]]).A, ([[OPENED_TY]]).A>
// CHECK: [[STEP3:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[STEP2]]
// CHECK: [[OPENED_COPY:%[0-9]+]] = alloc_stack $[[OPENED_TY]]
// CHECK: copy_addr [[OPENED]] to [initialization] [[OPENED_COPY]] : $*[[OPENED_TY]]
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $[[OPENED_TY]], #P.subscript!getter : <Self where Self : P> (Self) -> ((Self.A...) -> ()) -> ()
// CHECK: apply [[WITNESS]]<[[OPENED_TY]]>([[STEP3]], [[OPENED_COPY]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1> (@guaranteed Array<τ_0_0>) -> () for <τ_0_0.A, τ_0_0.A>, @in_guaranteed τ_0_0) -> ()
func testCovariantAssocSubscript8(p: any P) {
  _ = p[covariantAssocSubscript8: { _ in }]
}

// -----------------------------------------------------------------------------
// Covariant dependent member type erasure
// -----------------------------------------------------------------------------

class Class {}
protocol P2: P where A: Class & P {}

// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype36testCovariantAssocMethod1Constrained2p2yAA2P2_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P2 to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P2
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P2, #P.covariantAssocMethod1 : <Self where Self : P> (Self) -> () -> Self.A
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P2>(%{{[0-9]+}}, [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.A
// CHECK: init_existential_ref %{{[0-9]+}} : $(@opened([[OPENED_ID]]) P2).A : $(@opened([[OPENED_ID]]) P2).A, $Class & P
// CHECK: debug_value %{{[0-9]+}} : $Class & P, let, name "x"
func testCovariantAssocMethod1Constrained(p2: any P2) {
  let x = p2.covariantAssocMethod1()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype36testCovariantAssocMethod2Constrained2p2yAA2P2_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P2 to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P2
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P2, #P.covariantAssocMethod2 : <Self where Self : P> (Self) -> () -> Self.A?
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P2>(%{{[0-9]+}}, [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out Optional<τ_0_0.A>
// CHECK: init_existential_ref %{{[0-9]+}} : $(@opened([[OPENED_ID]]) P2).A : $(@opened([[OPENED_ID]]) P2).A, $Class & P
// CHECK: debug_value %{{[0-9]+}} : $Optional<Class & P>, let, name "x"
func testCovariantAssocMethod2Constrained(p2: any P2) {
  let x = p2.covariantAssocMethod2()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype36testCovariantAssocMethod3Constrained2p2yAA2P2_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P2 to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P2
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P2, #P.covariantAssocMethod3 : <Self where Self : P> (Self) -> () -> Self.A.Type
// CHECK: [[META:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P2>([[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @thick τ_0_0.A.Type
// CHECK: [[EXIST_META:%[0-9]+]] = init_existential_metatype [[META]] : $@thick ((@opened([[OPENED_ID]]) P2).A).Type, $@thick (Class & P).Type
// CHECK: debug_value [[EXIST_META]] : $@thick (Class & P).Type, let, name "x"
func testCovariantAssocMethod3Constrained(p2: any P2) {
  let x = p2.covariantAssocMethod3()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype36testCovariantAssocMethod4Constrained2p2yAA2P2_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P2 to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P2
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P2, #P.covariantAssocMethod4 : <Self where Self : P> (Self) -> () -> (Self.A, Self.A)
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P2>(%{{[0-9]+}}, %{{[0-9]+}}, [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> (@out τ_0_0.A, @out τ_0_0.A)
// CHECK: init_existential_ref %{{[0-9]+}} : $(@opened([[OPENED_ID]]) P2).A : $(@opened([[OPENED_ID]]) P2).A, $Class & P
// CHECK: init_existential_ref %{{[0-9]+}} : $(@opened([[OPENED_ID]]) P2).A : $(@opened([[OPENED_ID]]) P2).A, $Class & P
// CHECK: debug_value %{{[0-9]+}} : $(Class & P, Class & P), let, name "x"
func testCovariantAssocMethod4Constrained(p2: any P2) {
  let x = p2.covariantAssocMethod4()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype36testCovariantAssocMethod5Constrained2p2yAA2P2_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P2 to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P2
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P2, #P.covariantAssocMethod5 : <Self where Self : P> (Self) -> () -> Array<Self.A>
// CHECK: [[ARRAY:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P2>([[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned Array<τ_0_0.A>
// CHECK: [[ARRAY_UPCAST_FN:%[0-9]+]] = function_ref @$ss15_arrayForceCastySayq_GSayxGr0_lF : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: [[RESULT:%[0-9]+]] = apply [[ARRAY_UPCAST_FN]]<(@opened([[OPENED_ID]]) P2).A, Class & P>([[ARRAY]]) : $@convention(thin) <τ_0_0, τ_0_1> (@guaranteed Array<τ_0_0>) -> @owned Array<τ_0_1>
// CHECK: debug_value %{{[0-9]+}} : $Array<Class & P>, let, name "x"
func testCovariantAssocMethod5Constrained(p2: any P2) {
  let x = p2.covariantAssocMethod5()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype36testCovariantAssocMethod6Constrained2p2yAA2P2_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P2 to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P2
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P2, #P.covariantAssocMethod6 : <Self where Self : P> (Self) -> () -> [String : Self.A]
// CHECK: [[DICT:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P2>([[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @owned Dictionary<String, τ_0_0.A>
// CHECK: [[DICT_UPCAST_FN:%[0-9]+]] = function_ref @$ss17_dictionaryUpCastySDyq0_q1_GSDyxq_GSHRzSHR0_r2_lF : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: [[RESULT:%[0-9]+]] = apply [[DICT_UPCAST_FN]]<String, (@opened([[OPENED_ID]]) P2).A, String, Class & P>([[DICT]]) : $@convention(thin) <τ_0_0, τ_0_1, τ_0_2, τ_0_3 where τ_0_0 : Hashable, τ_0_2 : Hashable> (@guaranteed Dictionary<τ_0_0, τ_0_1>) -> @owned Dictionary<τ_0_2, τ_0_3>
// CHECK: debug_value %{{[0-9]+}} : $Dictionary<String, Class & P>, let, name "x"
func testCovariantAssocMethod6Constrained(p2: any P2) {
  let x = p2.covariantAssocMethod6()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype36testCovariantAssocMethod7Constrained2p2yAA2P2_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P2 to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P2
// CHECK: function_ref thunk for @callee_guaranteed (@guaranteed Class & P) -> ()
// CHECK-NEXT: [[THUNK1:%[0-9]+]] = function_ref @$s42existential_member_accesses_self_assoctype1P_AA5ClassCXcIgg_1AAaBPQzIegg_AA2P2RzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : P2> (@guaranteed τ_0_0.A, @noescape @callee_guaranteed (@guaranteed Class & P) -> ()) -> ()
// CHECK: [[STEP1:%[0-9]+]] = partial_apply [callee_guaranteed] [[THUNK1]]<@opened([[OPENED_ID]]) P2>
// CHECK: [[STEP2:%[0-9]+]] = convert_function [[STEP1]] : $@callee_guaranteed (@guaranteed (@opened([[OPENED_ID]]) P2).A) -> () to $@callee_guaranteed @substituted <τ_0_0 where τ_0_0 : _NativeClass> (@guaranteed τ_0_0) -> () for <(@opened([[OPENED_ID]]) P2).A>
// CHECK: [[STEP3:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[STEP2]]
// CHECK: [[STEP4:%[0-9]+]] = convert_function [[STEP3]] : $@noescape @callee_guaranteed @substituted <τ_0_0 where τ_0_0 : _NativeClass> (@guaranteed τ_0_0) -> () for <(@opened([[OPENED_ID]]) P2).A> to $@noescape @callee_guaranteed (@guaranteed (@opened([[OPENED_ID]]) P2).A) -> ()
// FIXME: 'A.P.A' is a rather weird way to print (@opened P2).A
// CHECK: function_ref thunk for @callee_guaranteed (@guaranteed A.P.A) -> ()
// CHECK: [[THUNK2:%[0-9]+]] = function_ref @$s1A42existential_member_accesses_self_assoctype1PPQzIgg_AEIegn_AB2P2RzlTR : $@convention(thin) <τ_0_0 where τ_0_0 : P2> (@in_guaranteed τ_0_0.A, @noescape @callee_guaranteed (@guaranteed τ_0_0.A) -> ()) -> ()
// CHECK: [[STEP5:%[0-9]+]] = partial_apply [callee_guaranteed] [[THUNK2]]<@opened([[OPENED_ID]]) P2>([[STEP4]])
// CHECK: [[STEP6:%[0-9]+]] = convert_function [[STEP5]] : $@callee_guaranteed (@in_guaranteed (@opened([[OPENED_ID]]) P2).A) -> () to $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <(@opened([[OPENED_ID]]) P2).A>
// CHECK: [[STEP7:%[0-9]+]] = convert_escape_to_noescape [not_guaranteed] [[STEP6]]
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P2, #P.covariantAssocMethod7 : <Self where Self : P> (Self) -> ((Self.A) -> ()) -> ()
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P2>([[STEP7]], [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_0.A>, @in_guaranteed τ_0_0) -> ()
func testCovariantAssocMethod7Constrained(p2: any P2) {
  p2.covariantAssocMethod7 { _ in }
}

struct GenericStruct<T> {}
protocol P3: P where A == Bool {
  func contravariantAssocMethod1(_: A)
  func invariantAssocMethod1() -> GenericStruct<A>
}

// -----------------------------------------------------------------------------
// Concrete dependent member types
// -----------------------------------------------------------------------------

// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype33testCovariantAssocMethod1Concrete2p3yAA2P3_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P3 to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P3
// CHECK: [[BOOL_ADDR:%[0-9]+]] = alloc_stack $Bool
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P3, #P.covariantAssocMethod1 : <Self where Self : P> (Self) -> () -> Self.A
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P3>([[BOOL_ADDR]], [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.A
// CHECK: [[BOOL:%[0-9]+]] = load [trivial] [[BOOL_ADDR]] : $*Bool
// CHECK: debug_value [[BOOL]] : $Bool, let, name "x"
func testCovariantAssocMethod1Concrete(p3: any P3) {
  let x = p3.covariantAssocMethod1()
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype37testContravariantAssocMethod1Concrete2p3yAA2P3_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P3 to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P3
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P3, #P3.contravariantAssocMethod1 : <Self where Self : P3> (Self) -> (Self.A) -> ()
// CHECK: apply [[WITNESS]]<@opened([[OPENED_ID]]) P3>(%{{[0-9]+}}, [[OPENED]]) : $@convention(witness_method: P3) <τ_0_0 where τ_0_0 : P3> (Bool, @in_guaranteed τ_0_0) -> ()
func testContravariantAssocMethod1Concrete(p3: any P3) {
  p3.contravariantAssocMethod1(true)
}
// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype33testInvariantAssocMethod1Concrete2p3yAA2P3_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P3 to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P3
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P3, #P3.invariantAssocMethod1 : <Self where Self : P3> (Self) -> () -> GenericStruct<Self.A>
// CHECK: [[RESULT:%[0-9]+]] = apply [[WITNESS]]<@opened([[OPENED_ID]]) P3>([[OPENED]]) : $@convention(witness_method: P3) <τ_0_0 where τ_0_0 : P3> (@in_guaranteed τ_0_0) -> GenericStruct<Bool>
// CHECK: debug_value [[RESULT]] : $GenericStruct<Bool>, let, name "x"
func testInvariantAssocMethod1Concrete(p3: any P3) {
  let x = p3.invariantAssocMethod1()
}

// -----------------------------------------------------------------------------
//  Covariant dependent member type erasure in concrete dependent member type
// -----------------------------------------------------------------------------

protocol P4: P where A == (B, B) {
  associatedtype B: P4
}

// CHECK-LABEL: sil hidden [ossa] @$s42existential_member_accesses_self_assoctype42testCovariantAssocMethod1PartiallyConcrete2p4yAA2P4_p_tF
// CHECK: alloc_stack [lexical] $(P4, P4), let, name "x"
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access %0 : $*P4 to $*@opened([[OPENED_ID:"[0-9A-F-]+"]]) P4
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $@opened([[OPENED_ID]]) P4, #P.covariantAssocMethod1 : <Self where Self : P> (Self) -> () -> Self.A
// CHECK: apply %4<@opened([[OPENED_ID]]) P4>(%{{[0-9]+}}, [[OPENED]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.A
// CHECK: init_existential_addr %{{[0-9]+}} : $*P4, $(@opened([[OPENED_ID]]) P4).B
// CHECK: init_existential_addr %{{[0-9]+}} : $*P4, $(@opened([[OPENED_ID]]) P4).B
func testCovariantAssocMethod1PartiallyConcrete(p4: any P4) {
  let x = p4.covariantAssocMethod1()
}
