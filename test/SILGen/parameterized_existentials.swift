// RUN: %target-swift-emit-silgen -module-name parameterized -enable-parameterized-existential-types %s | %FileCheck %s

protocol P<T, U, V> {
  associatedtype T
  associatedtype U
  associatedtype V
}

protocol Q<X, Y, Z> : P {
  associatedtype X
  associatedtype Y
  associatedtype Z
}

struct S: Q {
  typealias T = Int
  typealias U = String
  typealias V = Float

  typealias X = Int
  typealias Y = String
  typealias Z = Float
}

// CHECK-LABEL: sil hidden [ossa] @$s13parameterized6upcastyAA1P_pAA1SVF : $@convention(thin) (S) -> @out P {
func upcast(_ x: S) -> any P {
  // CHECK: bb0([[RESULT_PARAM:%.*]] : $*P, [[CONCRETE_VAL:%.*]] : $S):
  // CHECK: [[Q_INT_STRING_FLOAT:%.*]] = alloc_stack $Q<Int, String, Float>
  // CHECK: [[INIT_Q_INT_STRING_FLOAT:%.*]] = init_existential_addr [[Q_INT_STRING_FLOAT]] : $*Q<Int, String, Float>, $S
  // CHECK: store [[CONCRETE_VAL]] to [trivial] [[INIT_Q_INT_STRING_FLOAT]] : $*S
  // CHECK: [[OPEN_Q_INT_STRING_FLOAT:%.*]] = open_existential_addr immutable_access [[Q_INT_STRING_FLOAT]] : $*Q<Int, String, Float> to $*[[OPENED_Q_INT_STRING_FLOAT:@opened(.*) Q<Int, String, Float>]]
  // CHECK: [[RESULT_INIT:%.*]] = init_existential_addr [[RESULT_PARAM]] : $*P, $[[OPENED_Q_INT_STRING_FLOAT]]
  // CHECK: copy_addr [[OPEN_Q_INT_STRING_FLOAT]] to [initialization] [[RESULT_INIT]] : $*[[OPENED_Q_INT_STRING_FLOAT]]

  return x as any Q<Int, String, Float> as any P
}

// CHECK-LABEL: sil hidden [ossa] @$s13parameterized12upupupupcastyAA1P_pAA1SVF : $@convention(thin) (S) -> @out P {
func upupupupcast(_ x: S) -> any P {
  // CHECK: bb0([[RESULT_PARAM:%.*]] : $*P, [[CONCRETE_VAL:%.*]] : $S):

  // CHECK: [[P_INT_STRING_FLOAT:%.*]] = alloc_stack $P<Int, String, Float>
  // CHECK: [[INIT_INT_STRING_FLOAT:%.*]] = init_existential_addr [[P_INT_STRING_FLOAT]] : $*P<Int, String, Float>, $S
  // CHECK: store [[CONCRETE_VAL]] to [trivial] [[INIT_INT_STRING_FLOAT]] : $*S
  // CHECK: [[OPEN_INT_STRING_FLOAT:%.*]] = open_existential_addr immutable_access %3 : $*P<Int, String, Float> to $*[[OPENED_P_INT_STRING_FLOAT:@opened(.*) P<Int, String, Float>]]

  // CHECK: [[RESULT_INIT:%.*]] = init_existential_addr [[RESULT_PARAM]] : $*P, $[[OPENED_P_INT_STRING_FLOAT]]
  // CHECK: copy_addr [[OPEN_INT_STRING_FLOAT]] to [initialization] [[RESULT_INIT]] : $*[[OPENED_P_INT_STRING_FLOAT]]
  return x as any P<Int, String, Float> as any P
}

func use(_ k: (S) -> Void) {}

// CHECK-LABEL: sil hidden [ossa] @$s13parameterized11upcastInputyyF : $@convention(thin) () -> () {
func upcastInput() {
  // CHECK: [[P_FN:%.*]] = function_ref @$s13parameterized11upcastInputyyFyAA1P_pXEfU_ : $@convention(thin) (@in_guaranteed P) -> ()
  // CHECK: [[NOESCAPE_P_FN:%.*]] = convert_function [[P_FN]] : $@convention(thin) (@in_guaranteed P) -> () to $@convention(thin) @noescape (@in_guaranteed P) -> ()
  // CHECK: [[THICK_P_FN:%.*]] = thin_to_thick_function [[NOESCAPE_P_FN]] : $@convention(thin) @noescape (@in_guaranteed P) -> () to $@noescape @callee_guaranteed (@in_guaranteed P) -> ()
  // CHECK: [[S_TO_P_THUNK_FN:%.*]] = function_ref @$s13parameterized1P_pIgn_AA1SVIegy_TR : $@convention(thin) (S, @noescape @callee_guaranteed (@in_guaranteed P) -> ()) -> ()
  // CHECK: [[PARTIAL_INT_THUNK_FN:%.*]] = partial_apply [callee_guaranteed] [[S_TO_P_THUNK_FN]]([[THICK_P_FN]]) : $@convention(thin) (S, @noescape @callee_guaranteed (@in_guaranteed P) -> ()) -> ()
  // CHECK: [[NOESCAPE_P_THUNK_FN:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_INT_THUNK_FN]] : $@callee_guaranteed (S) -> () to $@noescape @callee_guaranteed (S) -> ()
  // CHECK: [[USE_FN:%.*]] = function_ref @$s13parameterized3useyyyAA1SVXEF : $@convention(thin) (@noescape @callee_guaranteed (S) -> ()) -> ()
  // CHECK: {{%.*}} = apply [[USE_FN]]([[NOESCAPE_P_THUNK_FN]]) : $@convention(thin) (@noescape @callee_guaranteed (S) -> ()) -> ()

  use({ (p: any P) -> Void in })
}

func reuse(_ k: () -> any P<Int, String, Float>) {}

// CHECK-LABEL: sil hidden [ossa] @$s13parameterized12upcastResultyyF : $@convention(thin) () -> () {
func upcastResult() {
  // CHECK: [[RES_FN:%.*]] = function_ref @$s13parameterized12upcastResultyyFAA1SVyXEfU_ : $@convention(thin) () -> S
  // CHECK: [[NOESCAPE_RES_FN:%.*]] = convert_function [[RES_FN]] : $@convention(thin) () -> S to $@convention(thin) @noescape () -> S
  // CHECK: [[THICK_RES_FN:%.*]] = thin_to_thick_function [[NOESCAPE_RES_FN]] : $@convention(thin) @noescape () -> S to $@noescape @callee_guaranteed () -> S
  // CHECK: [[S_TO_P_RES_THUNK_FN:%.*]] = function_ref @$s13parameterized1SVIgd_AA1P_pySiSSSfXPIegr_TR : $@convention(thin) (@noescape @callee_guaranteed () -> S) -> @out P<Int, String, Float>
  // CHECK: [[PARTIAL_RES_THUNK_FN:%.*]] = partial_apply [callee_guaranteed] [[S_TO_P_RES_THUNK_FN]]([[THICK_RES_FN]]) : $@convention(thin) (@noescape @callee_guaranteed () -> S) -> @out P<Int, String, Float>
  // CHECK: [[NOESCAPE_RES_THUNK_FN:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_RES_THUNK_FN]] : $@callee_guaranteed () -> @out P<Int, String, Float> to $@noescape @callee_guaranteed () -> @out P<Int, String, Float>
  // CHECK: [[REUSE_FN:%.*]] = function_ref @$s13parameterized5reuseyyAA1P_pySiSSSfXPyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> @out P<Int, String, Float>) -> ()
  // CHECK: {{%.*}} = apply [[REUSE_FN]]([[NOESCAPE_RES_THUNK_FN]]) : $@convention(thin) (@noescape @callee_guaranteed () -> @out P<Int, String, Float>) -> ()

  reuse({ () -> S in S() })

  // CHECK: [[RES_Q_FN:%.*]] = function_ref @$s13parameterized12upcastResultyyFAA1Q_pySiSSSfXPyXEfU0_ : $@convention(thin) () -> @out Q<Int, String, Float>
  // CHECK: [[NOESCAPE_RES_Q_FN:%.*]] = convert_function [[RES_Q_FN]] : $@convention(thin) () -> @out Q<Int, String, Float> to $@convention(thin) @noescape () -> @out Q<Int, String, Float>
  // CHECK: [[THICK_NOESCAPE_RES_Q_FN:%.*]] = thin_to_thick_function [[NOESCAPE_RES_Q_FN]] : $@convention(thin) @noescape () -> @out Q<Int, String, Float> to $@noescape @callee_guaranteed () -> @out Q<Int, String, Float>
  // CHECK: [[P_TO_Q_RES_THUNK_FN:%.*]] = function_ref @$s13parameterized1Q_pySiSSSfXPIgr_AA1P_pySiSSSfXPIegr_TR : $@convention(thin) (@noescape @callee_guaranteed () -> @out Q<Int, String, Float>) -> @out P<Int, String, Float>
  // CHECK: [[PARTIAL_P_TO_Q_RES_THUNK_FN:%.*]] = partial_apply [callee_guaranteed] [[P_TO_Q_RES_THUNK_FN]]([[THICK_NOESCAPE_RES_Q_FN]]) : $@convention(thin) (@noescape @callee_guaranteed () -> @out Q<Int, String, Float>) -> @out P<Int, String, Float>
  // CHECK: [[NOESCAPE_PARTIAL_P_TO_Q_RES_THUNK_FN:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_P_TO_Q_RES_THUNK_FN]] : $@callee_guaranteed () -> @out P<Int, String, Float> to $@noescape @callee_guaranteed () -> @out P<Int, String, Float>
  // CHECK: [[REUSE_FN:%.*]] = function_ref @$s13parameterized5reuseyyAA1P_pySiSSSfXPyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> @out P<Int, String, Float>) -> ()
  // CHECK: {{%.*}} = apply [[REUSE_FN]]([[NOESCAPE_PARTIAL_P_TO_Q_RES_THUNK_FN]]) : $@convention(thin) (@noescape @callee_guaranteed () -> @out P<Int, String, Float>) -> ()

  reuse({ () -> any Q<Int, String, Float> in S() })
}
