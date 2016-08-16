// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

struct MyMetatypeIsThin {}

// CHECK-LABEL: sil hidden @_TF17reabstract_lvalue19consumeGenericInOut{{.*}} : $@convention(thin) <T> (@inout T) -> ()
func consumeGenericInOut<T>(_ x: inout T) {}

// CHECK-LABEL: sil hidden @_TF17reabstract_lvalue9transformFSiSd : $@convention(thin) (Int) -> Double
func transform(_ i: Int) -> Double {
  return Double(i)
}

// CHECK-LABEL: sil hidden @_TF17reabstract_lvalue23reabstractFunctionInOutFT_T_ : $@convention(thin) () -> ()
func reabstractFunctionInOut() {
  // CHECK: [[BOX:%.*]] = alloc_box $@callee_owned (Int) -> Double
  // CHECK: [[PB:%.*]] = project_box [[BOX]]
  // CHECK: [[ARG:%.*]] = function_ref @_TF17reabstract_lvalue9transformFSiSd
  // CHECK: [[THICK_ARG:%.*]] = thin_to_thick_function [[ARG]]
  // CHECK: store [[THICK_ARG:%.*]] to [[PB]]
  // CHECK: [[FUNC:%.*]] = function_ref @_TF17reabstract_lvalue19consumeGenericInOut
  // CHECK: [[ABSTRACTED_BOX:%.*]] = alloc_stack $@callee_owned (@in Int) -> @out Double
  // CHECK: [[THICK_ARG:%.*]] = load [[PB]]
  // CHECK: strong_retain [[THICK_ARG]]
  // CHECK: [[THUNK1:%.*]] = function_ref @_TTRXFo_dSi_dSd_XFo_iSi_iSd_
  // CHECK: [[ABSTRACTED_ARG:%.*]] = partial_apply [[THUNK1]]([[THICK_ARG]])
  // CHECK: store [[ABSTRACTED_ARG]] to [[ABSTRACTED_BOX]]
  // CHECK: apply [[FUNC]]<(Int) -> Double>([[ABSTRACTED_BOX]])
  // CHECK: [[NEW_ABSTRACTED_ARG:%.*]] = load [[ABSTRACTED_BOX]]
  // CHECK: [[THUNK2:%.*]] = function_ref @_TTRXFo_iSi_iSd_XFo_dSi_dSd_
  // CHECK: [[NEW_ARG:%.*]] = partial_apply [[THUNK2]]([[NEW_ABSTRACTED_ARG]])
  var minimallyAbstracted = transform
  consumeGenericInOut(&minimallyAbstracted)
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_dSi_dSd_XFo_iSi_iSd_ : $@convention(thin) (@in Int, @owned @callee_owned (Int) -> Double) -> @out Double
// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_iSi_iSd_XFo_dSi_dSd_ : $@convention(thin) (Int, @owned @callee_owned (@in Int) -> @out Double) -> Double

// CHECK-LABEL: sil hidden @_TF17reabstract_lvalue23reabstractMetatypeInOutFT_T_ : $@convention(thin) () -> ()
func reabstractMetatypeInOut() {
  var thinMetatype = MyMetatypeIsThin.self
  // CHECK: [[FUNC:%.*]] = function_ref @_TF17reabstract_lvalue19consumeGenericInOut
  // CHECK: [[BOX:%.*]] = alloc_stack $@thick MyMetatypeIsThin.Type
  // CHECK: [[THICK:%.*]] = metatype $@thick MyMetatypeIsThin.Type
  // CHECK: store [[THICK]] to [[BOX]]
  // CHECK: apply [[FUNC]]<MyMetatypeIsThin.Type>([[BOX]])
  consumeGenericInOut(&thinMetatype)
}
