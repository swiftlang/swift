// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature CalledAttribute %s | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute

struct Big {
  var a, b, c, d: Int
}

func identity<T>(_ f: @escaping (T) -> Void) -> (T) -> Void { f }

func consumeCalledOnce(_ f: @called(once) (Big) -> Void, _ value: Big) {
  f(value)
}

func acceptGenericCalledOnce<T>(_ f: @called(once) (T) -> Void, _ value: T) {
  f(value)
}

// CHECK-LABEL: sil hidden [ossa] @$s17called_once_thunk21genericMakeCalledOnceyyxXOyxclF : $@convention(thin) <T> (@guaranteed @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T>) -> @owned @called(once) @callee_owned @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T> {
// CHECK:      [[CONVERTED:%.*]] = convert_function {{%.*}} : $@callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T> to $@callee_guaranteed (@in_guaranteed T) -> ()
// CHECK:      [[THUNK:%.*]] = function_ref @$sxIegn_xIeOxn_lTR : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0, @guaranteed @callee_guaranteed (@in_guaranteed τ_0_0) -> ()) -> ()
// CHECK-NEXT: [[CLOSURE:%.*]] = partial_apply [called_once] [[THUNK]]<T>([[CONVERTED]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0, @guaranteed @callee_guaranteed (@in_guaranteed τ_0_0) -> ()) -> ()
// CHECK:      return {{%.*}} : $@called(once) @callee_owned @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <T>
// CHECK: } // end sil function '$s17called_once_thunk21genericMakeCalledOnceyyxXOyxclF'
//
// The thunk itself is a bare, uninstantiated forwarder -- `@called(once)`
// only attaches once it's captured by the `partial_apply` above.
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sxIegn_xIeOxn_lTR : $@convention(thin) <T> (@in_guaranteed T, @guaranteed @callee_guaranteed (@in_guaranteed T) -> ()) -> () {
// CHECK: bb0([[ARG:%.*]] : $*T, [[FN:%.*]] : @guaranteed $@callee_guaranteed (@in_guaranteed T) -> ()):
// CHECK-NEXT: apply [[FN]]([[ARG]]) : $@callee_guaranteed (@in_guaranteed T) -> ()
// CHECK: } // end sil function '$sxIegn_xIeOxn_lTR'
func genericMakeCalledOnce<T>(_ f: @escaping (T) -> Void) -> @called(once) (T) -> Void {
  return f
}

// CHECK-LABEL: sil hidden [ossa] @$s17called_once_thunk22makeCalledOnceEscapingyyAA3BigVXOyADcF : $@convention(thin) (@guaranteed @callee_guaranteed (Big) -> ()) -> @owned @called(once) @callee_owned (Big) -> () {
// CHECK: [[LAST_THUNK:%.*]] = function_ref @$s17called_once_thunk3BigVIegy_ACIeOxy_TR : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> ()
// CHECK-NEXT: [[RESULT:%.*]] = partial_apply [called_once] [[LAST_THUNK]]({{%.*}}) : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> ()
// CHECK-NEXT: return [[RESULT]] : $@called(once) @callee_owned (Big) -> ()
// CHECK: } // end sil function '$s17called_once_thunk22makeCalledOnceEscapingyyAA3BigVXOyADcF'
//
// None of the generated reabstraction thunks themselves carry
// `@called(once)` in their own declared type.
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s17called_once_thunk3BigVIegy_ACIeOxy_TR : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> () {
// CHECK-NOT: @called(once)
// CHECK: } // end sil function '$s17called_once_thunk3BigVIegy_ACIeOxy_TR'
func makeCalledOnceEscaping(_ f: @escaping (Big) -> Void) -> @called(once) (Big) -> Void {
  return identity(f)
}

// CHECK-LABEL: sil hidden [ossa] @$s17called_once_thunk34testNoEscapeConversionThroughThunkyyyAA3BigVc_ADtF : $@convention(thin) (@guaranteed @callee_guaranteed (Big) -> (), Big) -> () {
// CHECK: [[THUNK:%.*]] = function_ref @$s17called_once_thunk3BigVIegy_ACIeOxy_TR : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> ()
// CHECK-NEXT: [[ESCAPING:%.*]] = partial_apply [called_once] [[THUNK]]({{%.*}}) : $@convention(thin) (Big, @guaranteed @callee_guaranteed (Big) -> ()) -> ()
// CHECK-NEXT: [[NOESCAPE:%.*]] = convert_escape_to_noescape [[ESCAPING]] : $@called(once) @callee_owned (Big) -> () to $@noescape @called(once) @callee_owned (Big) -> ()
// CHECK: function_ref @$s17called_once_thunk17consumeCalledOnceyyyAA3BigVXEn_ADtF
// CHECK-NEXT: apply {{%.*}}([[NOESCAPE]], {{%.*}}) : $@convention(thin) (@owned @noescape @called(once) @callee_owned (Big) -> (), Big) -> ()
// CHECK: } // end sil function '$s17called_once_thunk34testNoEscapeConversionThroughThunkyyyAA3BigVc_ADtF'
func testNoEscapeConversionThroughThunk(_ f: @escaping (Big) -> Void, _ big: Big) {
  consumeCalledOnce(identity(f), big)
}

// CHECK-LABEL: sil hidden [ossa] @$s17called_once_thunk30testGenericCalledOnceParameteryyyAA3BigVXEn_ADtF : $@convention(thin) (@owned @noescape @called(once) @callee_owned (Big) -> (), Big) -> () {
// CHECK: [[F_PROJ:%.*]] = project_box {{.*}} : ${ var @noescape @called(once) @callee_owned (Big) -> () }, 0
// CHECK: [[F_ACCESS:%.*]] = begin_access [deinit] [unknown] [[F_PROJ]] : $*@noescape @called(once) @callee_owned (Big) -> ()
// CHECK: [[F_ADDR:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[F_ACCESS]] : $*@noescape @called(once) @callee_owned (Big) -> ()
// CHECK: [[F_VALUE:%.*]] = load [copy] [[F_ADDR]] : $*@noescape @called(once) @callee_owned (Big) -> ()
// CHECK: [[THUNK:%.*]] = function_ref @$s17called_once_thunk3BigVIOxy_ACIeOxn_TR
// CHECK: partial_apply [called_once] [[THUNK]]([[F_VALUE]]) : $@convention(thin) (@in_guaranteed Big, @owned @noescape @called(once) @callee_owned (Big) -> ()) -> ()
// CHECK: function_ref @$s17called_once_thunk23acceptGenericCalledOnceyyyxXEn_xtlF
// CHECK: end_access [[F_ACCESS]] : $*@noescape @called(once) @callee_owned (Big) -> ()
// CHECK: } // end sil function '$s17called_once_thunk30testGenericCalledOnceParameteryyyAA3BigVXEn_ADtF'
func testGenericCalledOnceParameter(_ f: @called(once) (Big) -> Void, _ big: Big) {
  acceptGenericCalledOnce(f, big)
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s17called_once_thunk3BigVIOxy_ACIeOxn_TR : $@convention(thin) (@in_guaranteed Big, @owned @noescape @called(once) @callee_owned (Big) -> ()) -> () {
// CHECK: bb0([[ARG:%.*]] : $*Big, [[FN:%.*]] : @owned $@noescape @called(once) @callee_owned (Big) -> ()):
// CHECK: apply [[FN]]({{%.*}}) : $@noescape @called(once) @callee_owned (Big) -> ()
// CHECK: } // end sil function '$s17called_once_thunk3BigVIOxy_ACIeOxn_TR'
