// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature OnceFunctions %s | %FileCheck %s

// REQUIRES: swift_feature_OnceFunctions

struct Big {
  var a, b, c, d: Int
}

func identity<T>(_ f: @escaping (T) -> Void) -> (T) -> Void { f }

func consumeCalledExactlyOnce(_ f: @once (Big) -> Void, _ value: Big) {
  f(value)
}

func acceptGenericCalledExactlyOnce<T>(_ f: @once (T) -> Void, _ value: T) {
  f(value)
}

// CHECK-LABEL: sil hidden [ossa] @$s15once_attr_thunk28genericMakeCalledExactlyOnceyyxXOyxclF : $@convention(thin) <T> (@guaranteed @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T>) -> @owned @once @callee_owned @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T> {
// CHECK:      [[CONVERTED:%.*]] = convert_function {{%.*}} : $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T> to $@callee_guaranteed (@in_guaranteed T) -> ()
// CHECK:      [[THUNK:%.*]] = function_ref @$sxIegn_xIeOxn_lTR : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0, @guaranteed @callee_guaranteed (@in_guaranteed τ_0_0) -> ()) -> ()
// CHECK-NEXT: [[CLOSURE:%.*]] = partial_apply [called_once] [[THUNK]]<T>([[CONVERTED]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0, @guaranteed @callee_guaranteed (@in_guaranteed τ_0_0) -> ()) -> ()
// CHECK:      return {{%.*}} : $@once @callee_owned @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T>
// CHECK: } // end sil function '$s15once_attr_thunk28genericMakeCalledExactlyOnceyyxXOyxclF'
//
// The thunk itself is a bare, uninstantiated forwarder -- exactly-once
// semantics only attach once it's captured by the `partial_apply` above.
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sxIegn_xIeOxn_lTR : $@convention(thin) <T> (@in_guaranteed T, @guaranteed @callee_guaranteed (@in_guaranteed T) -> ()) -> () {
// CHECK: bb0([[ARG:%.*]] : $*T, [[FN:%.*]] : @guaranteed $@callee_guaranteed (@in_guaranteed T) -> ()):
// CHECK-NEXT: apply [[FN]]([[ARG]]) : $@callee_guaranteed (@in_guaranteed T) -> ()
// CHECK: } // end sil function '$sxIegn_xIeOxn_lTR'
func genericMakeCalledExactlyOnce<T>(_ f: @escaping (T) -> Void) -> @once (T) -> Void {
  return f
}

// CHECK-LABEL: sil hidden [ossa] @$s15once_attr_thunk29makeCalledExactlyOnceEscapingyyAA3BigVXOyADcF : $@convention(thin) (@guaranteed @callee_guaranteed (Big) -> ()) -> @owned @once @callee_owned (Big) -> () {
// CHECK: [[LAST_THUNK:%.*]] = function_ref @$s15once_attr_thunk3BigVIegy_ACIeOxy_TR : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> ()
// CHECK-NEXT: [[RESULT:%.*]] = partial_apply [called_once] [[LAST_THUNK]]({{%.*}}) : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> ()
// CHECK-NEXT: end_formal_scope
// CHECK-NEXT: return [[RESULT]] : $@once @callee_owned (Big) -> ()
// CHECK: } // end sil function '$s15once_attr_thunk29makeCalledExactlyOnceEscapingyyAA3BigVXOyADcF'
//
// None of the generated reabstraction thunks themselves carry exactly-once
// semantics in their own declared type.
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s15once_attr_thunk3BigVIegy_ACIeOxy_TR : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> () {
// CHECK-NOT: @once
// CHECK: } // end sil function '$s15once_attr_thunk3BigVIegy_ACIeOxy_TR'
func makeCalledExactlyOnceEscaping(_ f: @escaping (Big) -> Void) -> @once (Big) -> Void {
  return identity(f)
}

// CHECK-LABEL: sil hidden [ossa] @$s15once_attr_thunk34testNoEscapeConversionThroughThunkyyyAA3BigVc_ADtF : $@convention(thin) (@guaranteed @callee_guaranteed (Big) -> (), Big) -> () {
// CHECK: [[THUNK:%.*]] = function_ref @$s15once_attr_thunk3BigVIegy_ACIeOxy_TR : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> ()
// CHECK-NEXT: [[ESCAPING:%.*]] = partial_apply [called_once] [[THUNK]]({{%.*}}) : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> ()
// CHECK-NEXT: [[NOESCAPE:%.*]] = convert_escape_to_noescape [[ESCAPING]] : $@once @callee_owned (Big) -> () to $@noescape @once @callee_owned (Big) -> ()
// CHECK: function_ref @$s15once_attr_thunk24consumeCalledExactlyOnceyyyAA3BigVXEn_ADtF
// CHECK-NEXT: apply {{%.*}}([[NOESCAPE]], {{%.*}}) : $@convention(thin) (@owned @noescape @once @callee_owned (Big) -> (), Big) -> ()
// CHECK: } // end sil function '$s15once_attr_thunk34testNoEscapeConversionThroughThunkyyyAA3BigVc_ADtF'
func testNoEscapeConversionThroughThunk(_ f: @escaping (Big) -> Void, _ big: Big) {
  consumeCalledExactlyOnce(identity(f), big)
}

// CHECK-LABEL: sil hidden [ossa] @$s15once_attr_thunk37testGenericCalledExactlyOnceParameteryyyAA3BigVXEn_ADtF : $@convention(thin) (@owned @noescape @once @callee_owned (Big) -> (), Big) -> () {
// CHECK: [[F_PROJ:%.*]] = project_box {{.*}} : ${ var @noescape @once @callee_owned (Big) -> () }, 0
// CHECK: [[F_ACCESS:%.*]] = begin_access [deinit] [unknown] [[F_PROJ]] : $*@noescape @once @callee_owned (Big) -> ()
// CHECK: [[F_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[F_ACCESS]] : $*@noescape @once @callee_owned (Big) -> ()
// CHECK: [[F_VALUE:%.*]] = load [copy] [[F_ADDR]] : $*@noescape @once @callee_owned (Big) -> ()
// CHECK: [[THUNK:%.*]] = function_ref @$s15once_attr_thunk3BigVIOxy_ACIeOxn_TR
// CHECK: partial_apply [called_once] [[THUNK]]([[F_VALUE]]) : $@convention(thin) (@in_guaranteed Big, @owned @noescape @once @callee_owned (Big) -> ()) -> ()
// CHECK: function_ref @$s15once_attr_thunk30acceptGenericCalledExactlyOnceyyyxXEn_xtlF
// CHECK: end_access [[F_ACCESS]] : $*@noescape @once @callee_owned (Big) -> ()
// CHECK: } // end sil function '$s15once_attr_thunk37testGenericCalledExactlyOnceParameteryyyAA3BigVXEn_ADtF'
func testGenericCalledExactlyOnceParameter(_ f: @once (Big) -> Void, _ big: Big) {
  acceptGenericCalledExactlyOnce(f, big)
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s15once_attr_thunk3BigVIOxy_ACIeOxn_TR : $@convention(thin) (@in_guaranteed Big, @owned @noescape @once @callee_owned (Big) -> ()) -> () {
// CHECK: bb0([[ARG:%.*]] : $*Big, [[FN:%.*]] : @owned $@noescape @once @callee_owned (Big) -> ()):
// CHECK: apply [[FN]]({{%.*}}) : $@noescape @once @callee_owned (Big) -> ()
// CHECK: } // end sil function '$s15once_attr_thunk3BigVIOxy_ACIeOxn_TR'
