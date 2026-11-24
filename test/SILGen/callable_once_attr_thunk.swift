// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature OnceFunctions %s | %FileCheck %s

// REQUIRES: swift_feature_OnceFunctions

struct Big {
  var a, b, c, d: Int
}

func identity<T>(_ f: @escaping (T) -> Void) -> (T) -> Void { f }

func consumeCalledAtMostOnce(_ f: @callable(once) (Big) -> Void, _ value: Big) {
  f(value)
}

func acceptGenericCalledAtMostOnce<T>(_ f: @callable(once) (T) -> Void, _ value: T) {
  f(value)
}

// CHECK-LABEL: sil hidden [ossa] @$s24callable_once_attr_thunk27genericMakeCalledAtMostOnceyyxXOoyxclF : $@convention(thin) <T> (@guaranteed @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T>) -> @owned @callable(once) @callee_owned @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T> {
// CHECK:      [[CONVERTED:%.*]] = convert_function {{%.*}} : $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T> to $@callee_guaranteed (@in_guaranteed T) -> ()
// CHECK:      [[THUNK:%.*]] = function_ref @$sxIegn_xIeOoxn_lTR : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0, @guaranteed @callee_guaranteed (@in_guaranteed τ_0_0) -> ()) -> ()
// CHECK-NEXT: [[CLOSURE:%.*]] = partial_apply [called_at_most_once] [[THUNK]]<T>([[CONVERTED]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0, @guaranteed @callee_guaranteed (@in_guaranteed τ_0_0) -> ()) -> ()
// CHECK:      return {{%.*}} : $@callable(once) @callee_owned @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T>
// CHECK: } // end sil function '$s24callable_once_attr_thunk27genericMakeCalledAtMostOnceyyxXOoyxclF'
//
// The thunk itself is a bare, uninstantiated forwarder -- at-most-once
// only attaches once it's captured by the `partial_apply` above.
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sxIegn_xIeOoxn_lTR : $@convention(thin) <T> (@in_guaranteed T, @guaranteed @callee_guaranteed (@in_guaranteed T) -> ()) -> () {
// CHECK: bb0([[ARG:%.*]] : $*T, [[FN:%.*]] : @guaranteed $@callee_guaranteed (@in_guaranteed T) -> ()):
// CHECK-NEXT: apply [[FN]]([[ARG]]) : $@callee_guaranteed (@in_guaranteed T) -> ()
// CHECK: } // end sil function '$sxIegn_xIeOoxn_lTR'
func genericMakeCalledAtMostOnce<T>(_ f: @escaping (T) -> Void) -> @callable(once) (T) -> Void {
  return f
}

// CHECK-LABEL: sil hidden [ossa] @$s24callable_once_attr_thunk28makeCalledAtMostOnceEscapingyyAA3BigVXOoyADcF : $@convention(thin) (@guaranteed @callee_guaranteed (Big) -> ()) -> @owned @callable(once) @callee_owned (Big) -> () {
// CHECK: [[LAST_THUNK:%.*]] = function_ref @$s24callable_once_attr_thunk3BigVIegy_ACIeOoxy_TR : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> ()
// CHECK-NEXT: [[RESULT:%.*]] = partial_apply [called_at_most_once] [[LAST_THUNK]]({{%.*}}) : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> ()
// CHECK-NEXT: end_formal_scope
// CHECK-NEXT: return [[RESULT]] : $@callable(once) @callee_owned (Big) -> ()
// CHECK: } // end sil function '$s24callable_once_attr_thunk28makeCalledAtMostOnceEscapingyyAA3BigVXOoyADcF'
//
// None of the generated reabstraction thunks themselves carry
// at-most-once in their own declared type.
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s24callable_once_attr_thunk3BigVIegy_ACIeOoxy_TR : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> () {
// CHECK-NOT: @callable(once)
// CHECK: } // end sil function '$s24callable_once_attr_thunk3BigVIegy_ACIeOoxy_TR'
func makeCalledAtMostOnceEscaping(_ f: @escaping (Big) -> Void) -> @callable(once) (Big) -> Void {
  return identity(f)
}

// CHECK-LABEL: sil hidden [ossa] @$s24callable_once_attr_thunk34testNoEscapeConversionThroughThunkyyyAA3BigVc_ADtF : $@convention(thin) (@guaranteed @callee_guaranteed (Big) -> (), Big) -> () {
// CHECK: [[THUNK:%.*]] = function_ref @$s24callable_once_attr_thunk3BigVIegy_ACIeOoxy_TR : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> ()
// CHECK-NEXT: [[ESCAPING:%.*]] = partial_apply [called_at_most_once] [[THUNK]]({{%.*}}) : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> ()
// CHECK-NEXT: [[NOESCAPE:%.*]] = convert_escape_to_noescape [[ESCAPING]] : $@callable(once) @callee_owned (Big) -> () to $@noescape @callable(once) @callee_owned (Big) -> ()
// CHECK: function_ref @$s24callable_once_attr_thunk23consumeCalledAtMostOnceyyyAA3BigVXEn_ADtF
// CHECK-NEXT: apply {{%.*}}([[NOESCAPE]], {{%.*}}) : $@convention(thin) (@owned @noescape @callable(once) @callee_owned (Big) -> (), Big) -> ()
// CHECK: } // end sil function '$s24callable_once_attr_thunk34testNoEscapeConversionThroughThunkyyyAA3BigVc_ADtF'
func testNoEscapeConversionThroughThunk(_ f: @escaping (Big) -> Void, _ big: Big) {
  consumeCalledAtMostOnce(identity(f), big)
}

// CHECK-LABEL: sil hidden [ossa] @$s24callable_once_attr_thunk36testGenericCalledAtMostOnceParameteryyyAA3BigVXEn_ADtF : $@convention(thin) (@owned @noescape @callable(once) @callee_owned (Big) -> (), Big) -> () {
// CHECK: [[F_PROJ:%.*]] = project_box {{.*}} : ${ var @noescape @callable(once) @callee_owned (Big) -> () }, 0
// CHECK: [[F_ACCESS:%.*]] = begin_access [deinit] [unknown] [[F_PROJ]] : $*@noescape @callable(once) @callee_owned (Big) -> ()
// CHECK: [[F_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[F_ACCESS]] : $*@noescape @callable(once) @callee_owned (Big) -> ()
// CHECK: [[F_VALUE:%.*]] = load [copy] [[F_ADDR]] : $*@noescape @callable(once) @callee_owned (Big) -> ()
// CHECK: [[THUNK:%.*]] = function_ref @$s24callable_once_attr_thunk3BigVIOoxy_ACIeOoxn_TR
// CHECK: partial_apply [called_at_most_once] [[THUNK]]([[F_VALUE]]) : $@convention(thin) (@in_guaranteed Big, @owned @noescape @callable(once) @callee_owned (Big) -> ()) -> ()
// CHECK: function_ref @$s24callable_once_attr_thunk29acceptGenericCalledAtMostOnceyyyxXEn_xtlF
// CHECK: end_access [[F_ACCESS]] : $*@noescape @callable(once) @callee_owned (Big) -> ()
// CHECK: } // end sil function '$s24callable_once_attr_thunk36testGenericCalledAtMostOnceParameteryyyAA3BigVXEn_ADtF'
func testGenericCalledAtMostOnceParameter(_ f: @callable(once) (Big) -> Void, _ big: Big) {
  acceptGenericCalledAtMostOnce(f, big)
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s24callable_once_attr_thunk3BigVIOoxy_ACIeOoxn_TR : $@convention(thin) (@in_guaranteed Big, @owned @noescape @callable(once) @callee_owned (Big) -> ()) -> () {
// CHECK: bb0([[ARG:%.*]] : $*Big, [[FN:%.*]] : @owned $@noescape @callable(once) @callee_owned (Big) -> ()):
// CHECK: apply [[FN]]({{%.*}}) : $@noescape @callable(once) @callee_owned (Big) -> ()
// CHECK: } // end sil function '$s24callable_once_attr_thunk3BigVIOoxy_ACIeOoxn_TR'
