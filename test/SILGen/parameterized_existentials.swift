// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name parameterized -target %target-swift-5.7-abi-triple %s | %FileCheck %s

protocol P<T, U, V> {
  associatedtype T
  associatedtype U
  associatedtype V
}

protocol Q<T, U, V> : P {}

struct S: Q {
  typealias T = Int
  typealias U = String
  typealias V = Float
}

struct R<T, U, V> {
  var force: () -> any P<T, U, V>
  // CHECK-LABEL: sil hidden [ossa] @$s13parameterized1RV5forceACyxq_q0_GAA1P_px1TRts_q_1URtsq0_1VRtsXPyc_tcfC : $@convention(method) <T, U, V> (@owned @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2> () -> @out any P<τ_0_0, τ_0_1, τ_0_2> for <T, U, V>, @thin R<T, U, V>.Type) -> @owned R<T, U, V> {
}

// CHECK-LABEL: sil hidden [ossa] @$s13parameterized6upcastyAA1P_pAA1SVF : $@convention(thin) (S) -> @out any P {
func upcast(_ x: S) -> any P {
  // CHECK: bb0([[RESULT_PARAM:%.*]] : $*any P, [[CONCRETE_VAL:%.*]] : $S):
  // CHECK: [[Q_INT_STRING_FLOAT:%.*]] = alloc_stack $any Q<Int, String, Float>
  // CHECK: [[INIT_Q_INT_STRING_FLOAT:%.*]] = init_existential_addr [[Q_INT_STRING_FLOAT]] : $*any Q<Int, String, Float>, $S
  // CHECK: store [[CONCRETE_VAL]] to [trivial] [[INIT_Q_INT_STRING_FLOAT]] : $*S
  // CHECK: [[OPEN_Q_INT_STRING_FLOAT:%.*]] = open_existential_addr immutable_access [[Q_INT_STRING_FLOAT]] : $*any Q<Int, String, Float> to $*[[OPENED_Q_INT_STRING_FLOAT:@opened\(.*, any Q<Int, String, Float>\) Self]]
  // CHECK: [[RESULT_INIT:%.*]] = init_existential_addr [[RESULT_PARAM]] : $*any P, $[[OPENED_Q_INT_STRING_FLOAT]]
  // CHECK: copy_addr [[OPEN_Q_INT_STRING_FLOAT]] to [init] [[RESULT_INIT]] : $*[[OPENED_Q_INT_STRING_FLOAT]]

  return x as any Q<Int, String, Float> as any P
}

// CHECK-LABEL: sil hidden [ossa] @$s13parameterized12upupupupcastyAA1P_pAA1SVF : $@convention(thin) (S) -> @out any P {
func upupupupcast(_ x: S) -> any P {
  // CHECK: bb0([[RESULT_PARAM:%.*]] : $*any P, [[CONCRETE_VAL:%.*]] : $S):

  // CHECK: [[P_INT_STRING_FLOAT:%.*]] = alloc_stack $any P<Int, String, Float>
  // CHECK: [[INIT_INT_STRING_FLOAT:%.*]] = init_existential_addr [[P_INT_STRING_FLOAT]] : $*any P<Int, String, Float>, $S
  // CHECK: store [[CONCRETE_VAL]] to [trivial] [[INIT_INT_STRING_FLOAT]] : $*S
  // CHECK: [[OPEN_INT_STRING_FLOAT:%.*]] = open_existential_addr immutable_access %3 : $*any P<Int, String, Float> to $*[[OPENED_P_INT_STRING_FLOAT:@opened\(.*, any P<Int, String, Float>\) Self]]

  // CHECK: [[RESULT_INIT:%.*]] = init_existential_addr [[RESULT_PARAM]] : $*any P, $[[OPENED_P_INT_STRING_FLOAT]]
  // CHECK: copy_addr [[OPEN_INT_STRING_FLOAT]] to [init] [[RESULT_INIT]] : $*[[OPENED_P_INT_STRING_FLOAT]]
  return x as any P<Int, String, Float> as any P
}

func use(_ k: (S) -> Void) {}

// CHECK-LABEL: sil hidden [ossa] @$s13parameterized11upcastInputyyF : $@convention(thin) () -> () {
func upcastInput() {
  // CHECK: [[P_FN:%.*]] = function_ref @$s13parameterized11upcastInputyyFyAA1P_pXEfU_ : $@convention(thin) (@in_guaranteed any P) -> ()
  // CHECK: [[THICK_P_FN:%.*]] = thin_to_thick_function [[P_FN]] : $@convention(thin) (@in_guaranteed any P) -> () to $@noescape @callee_guaranteed (@in_guaranteed any P) -> ()
  // CHECK: [[S_TO_P_THUNK_FN:%.*]] = function_ref @$s13parameterized1P_pIgn_AA1SVIegy_TR : $@convention(thin) (S, @guaranteed @noescape @callee_guaranteed (@in_guaranteed any P) -> ()) -> ()
  // CHECK: [[PARTIAL_INT_THUNK_FN:%.*]] = partial_apply [callee_guaranteed] [[S_TO_P_THUNK_FN]]([[THICK_P_FN]]) : $@convention(thin) (S, @guaranteed @noescape @callee_guaranteed (@in_guaranteed any P) -> ()) -> ()
  // CHECK: [[NOESCAPE_P_THUNK_FN:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_INT_THUNK_FN]] : $@callee_guaranteed (S) -> () to $@noescape @callee_guaranteed (S) -> ()
  // CHECK: [[USE_FN:%.*]] = function_ref @$s13parameterized3useyyyAA1SVXEF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed (S) -> ()) -> ()
  // CHECK: {{%.*}} = apply [[USE_FN]]([[NOESCAPE_P_THUNK_FN]]) : $@convention(thin) (@guaranteed @noescape @callee_guaranteed (S) -> ()) -> ()

  use({ (p: any P) -> Void in })
}

func reuse(_ k: () -> any P<Int, String, Float>) {}

// CHECK-LABEL: sil hidden [ossa] @$s13parameterized12upcastResultyyF : $@convention(thin) () -> () {
func upcastResult() {
  // CHECK: [[RES_FN:%.*]] = function_ref @$s13parameterized12upcastResultyyFAA1SVyXEfU_ : $@convention(thin) () -> S
  // CHECK: [[THICK_RES_FN:%.*]] = thin_to_thick_function [[RES_FN]] : $@convention(thin) () -> S to $@noescape @callee_guaranteed () -> S
  // CHECK: [[S_TO_P_RES_THUNK_FN:%.*]] = function_ref @$s13parameterized1SVIgd_AA1P_pSi1TAaDPRts_SS1UAFRtsSf1VAFRtsXPIegr_TR : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> S) -> @out any P<Int, String, Float>
  // CHECK: [[PARTIAL_RES_THUNK_FN:%.*]] = partial_apply [callee_guaranteed] [[S_TO_P_RES_THUNK_FN]]([[THICK_RES_FN]]) : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> S) -> @out any P<Int, String, Float>
  // CHECK: [[NOESCAPE_RES_THUNK_FN:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_RES_THUNK_FN]] : $@callee_guaranteed () -> @out any P<Int, String, Float> to $@noescape @callee_guaranteed () -> @out any P<Int, String, Float>
  // CHECK: [[REUSE_FN:%.*]] = function_ref @$s13parameterized5reuseyyAA1P_pSi1TAaCPRts_SS1UAERtsSf1VAERtsXPyXEF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> @out any P<Int, String, Float>) -> ()
  // CHECK: {{%.*}} = apply [[REUSE_FN]]([[NOESCAPE_RES_THUNK_FN]]) : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> @out any P<Int, String, Float>) -> ()

  reuse({ () -> S in S() })

  // CHECK: [[RES_Q_FN:%.*]] = function_ref @$s13parameterized12upcastResultyyFAA1Q_pSi1TAA1PPRts_SS1UAFRtsSf1VAFRtsXPyXEfU0_ : $@convention(thin) () -> @out any Q<Int, String, Float>
  // CHECK: [[THICK_NOESCAPE_RES_Q_FN:%.*]] = thin_to_thick_function [[RES_Q_FN]] : $@convention(thin) () -> @out any Q<Int, String, Float> to $@noescape @callee_guaranteed () -> @out any Q<Int, String, Float>
  // CHECK: [[P_TO_Q_RES_THUNK_FN:%.*]] = function_ref @$s13parameterized1Q_pSi1TAA1PPRts_SS1UAERtsSf1VAERtsXPIgr_AaD_pSiAFRS_SSAHRSSfAJRSXPIegr_TR : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> @out any Q<Int, String, Float>) -> @out any P<Int, String, Float>
  // CHECK: [[PARTIAL_P_TO_Q_RES_THUNK_FN:%.*]] = partial_apply [callee_guaranteed] [[P_TO_Q_RES_THUNK_FN]]([[THICK_NOESCAPE_RES_Q_FN]]) : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> @out any Q<Int, String, Float>) -> @out any P<Int, String, Float>
  // CHECK: [[NOESCAPE_PARTIAL_P_TO_Q_RES_THUNK_FN:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_P_TO_Q_RES_THUNK_FN]] : $@callee_guaranteed () -> @out any P<Int, String, Float> to $@noescape @callee_guaranteed () -> @out any P<Int, String, Float>
  // CHECK: [[REUSE_FN:%.*]] = function_ref @$s13parameterized5reuseyyAA1P_pSi1TAaCPRts_SS1UAERtsSf1VAERtsXPyXEF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> @out any P<Int, String, Float>) -> ()
  // CHECK: {{%.*}} = apply [[REUSE_FN]]([[NOESCAPE_PARTIAL_P_TO_Q_RES_THUNK_FN]]) : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> @out any P<Int, String, Float>) -> ()

  reuse({ () -> any Q<Int, String, Float> in S() })
}

// CHECK-LABEL: sil hidden [ossa] @$s13parameterized5forceAA1P_px1TRts_q_1URtsq0_1VRtsXPyr1_lF : $@convention(thin) <T, U, V> () -> @out any P<T, U, V> {
func force<T, U, V>() -> any P<T, U, V> {
  return R(force: { force() }).force()
}

protocol Box<Contents> {
  associatedtype Contents

  var contents: Contents { get }
}

// CHECK-LABEL: sil hidden [ossa] @$s13parameterized13functionTypes3box5valueq_AA3Box_pq_xc8ContentsRts_XP_xtr0_lF : $@convention(thin) <T, U> (@in_guaranteed any Box<(T) -> U>, @in_guaranteed T) -> @out U {
func functionTypes<T, U>(box: any Box<(T) -> U>, value: T) -> U {
  return box.contents(value)
}
