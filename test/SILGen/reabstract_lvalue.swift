
// RUN: %target-swift-emit-silgen -module-name reabstract_lvalue -enable-sil-ownership %s | %FileCheck %s

struct MyMetatypeIsThin {}

// CHECK-LABEL: sil hidden @$s17reabstract_lvalue19consumeGenericInOut{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T> (@inout T) -> ()
func consumeGenericInOut<T>(_ x: inout T) {}

// CHECK-LABEL: sil hidden @$s17reabstract_lvalue9transformySdSiF : $@convention(thin) (Int) -> Double
func transform(_ i: Int) -> Double {
  return Double(i)
}

// CHECK-LABEL: sil hidden @$s17reabstract_lvalue0A13FunctionInOutyyF : $@convention(thin) () -> ()
func reabstractFunctionInOut() {
  // CHECK: [[BOX:%.*]] = alloc_box ${ var @callee_guaranteed (Int) -> Double }
  // CHECK: [[PB:%.*]] = project_box [[BOX]]
  // CHECK: [[ARG:%.*]] = function_ref @$s17reabstract_lvalue9transformySdSiF
  // CHECK: [[THICK_ARG:%.*]] = thin_to_thick_function [[ARG]]
  // CHECK: store [[THICK_ARG:%.*]] to [init] [[PB]]
  // CHECK:  [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB]] : $*@callee_guaranteed (Int) -> Double
  // CHECK: [[ABSTRACTED_BOX:%.*]] = alloc_stack $@callee_guaranteed (@in_guaranteed Int) -> @out Double
  // CHECK: [[THICK_ARG:%.*]] = load [copy] [[WRITE]]
  // CHECK: [[THUNK1:%.*]] = function_ref @$sSiSdIegyd_SiSdIegnr_TR
  // CHECK: [[ABSTRACTED_ARG:%.*]] = partial_apply [callee_guaranteed] [[THUNK1]]([[THICK_ARG]])
  // CHECK: store [[ABSTRACTED_ARG]] to [init] [[ABSTRACTED_BOX]]
  // CHECK: [[FUNC:%.*]] = function_ref @$s17reabstract_lvalue19consumeGenericInOut{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[FUNC]]<(Int) -> Double>([[ABSTRACTED_BOX]])
  // CHECK: [[NEW_ABSTRACTED_ARG:%.*]] = load [take] [[ABSTRACTED_BOX]]
  // CHECK: [[THUNK2:%.*]] = function_ref @$sSiSdIegnr_SiSdIegyd_TR
  // CHECK: [[NEW_ARG:%.*]] = partial_apply [callee_guaranteed] [[THUNK2]]([[NEW_ABSTRACTED_ARG]])
  var minimallyAbstracted = transform
  consumeGenericInOut(&minimallyAbstracted)
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sSiSdIegyd_SiSdIegnr_TR : $@convention(thin) (@in_guaranteed Int, @guaranteed @callee_guaranteed (Int) -> Double) -> @out Double
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sSiSdIegnr_SiSdIegyd_TR : $@convention(thin) (Int, @guaranteed @callee_guaranteed (@in_guaranteed Int) -> @out Double) -> Double

// CHECK-LABEL: sil hidden @$s17reabstract_lvalue0A13MetatypeInOutyyF : $@convention(thin) () -> ()
func reabstractMetatypeInOut() {
  var thinMetatype = MyMetatypeIsThin.self
  // CHECK: [[BOX:%.*]] = alloc_stack $@thick MyMetatypeIsThin.Type
  // CHECK: [[THICK:%.*]] = metatype $@thick MyMetatypeIsThin.Type
  // CHECK: store [[THICK]] to [trivial] [[BOX]]
  // CHECK: [[FUNC:%.*]] = function_ref @$s17reabstract_lvalue19consumeGenericInOut{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[FUNC]]<MyMetatypeIsThin.Type>([[BOX]])
  consumeGenericInOut(&thinMetatype)
}
