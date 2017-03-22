// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

struct MyMetatypeIsThin {}

// CHECK-LABEL: sil hidden @_T017reabstract_lvalue19consumeGenericInOut{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T> (@inout T) -> ()
func consumeGenericInOut<T>(_ x: inout T) {}

// CHECK-LABEL: sil hidden @_T017reabstract_lvalue9transformSdSiF : $@convention(thin) (Int) -> Double
func transform(_ i: Int) -> Double {
  return Double(i)
}

// CHECK-LABEL: sil hidden @_T017reabstract_lvalue0A13FunctionInOutyyF : $@convention(thin) () -> ()
func reabstractFunctionInOut() {
  // CHECK: [[BOX:%.*]] = alloc_box ${ var @callee_owned (Int) -> Double }
  // CHECK: [[PB:%.*]] = project_box [[BOX]]
  // CHECK: [[ARG:%.*]] = function_ref @_T017reabstract_lvalue9transformSdSiF
  // CHECK: [[THICK_ARG:%.*]] = thin_to_thick_function [[ARG]]
  // CHECK: store [[THICK_ARG:%.*]] to [init] [[PB]]
  // CHECK: [[FUNC:%.*]] = function_ref @_T017reabstract_lvalue19consumeGenericInOut{{[_0-9a-zA-Z]*}}F
  // CHECK: [[ABSTRACTED_BOX:%.*]] = alloc_stack $@callee_owned (@in Int) -> @out Double
  // CHECK: [[THICK_ARG:%.*]] = load [copy] [[PB]]
  // CHECK: [[THUNK1:%.*]] = function_ref @_T0SiSdIxyd_SiSdIxir_TR
  // CHECK: [[ABSTRACTED_ARG:%.*]] = partial_apply [[THUNK1]]([[THICK_ARG]])
  // CHECK: store [[ABSTRACTED_ARG]] to [init] [[ABSTRACTED_BOX]]
  // CHECK: apply [[FUNC]]<(Int) -> Double>([[ABSTRACTED_BOX]])
  // CHECK: [[NEW_ABSTRACTED_ARG:%.*]] = load [take] [[ABSTRACTED_BOX]]
  // CHECK: [[THUNK2:%.*]] = function_ref @_T0SiSdIxir_SiSdIxyd_TR
  // CHECK: [[NEW_ARG:%.*]] = partial_apply [[THUNK2]]([[NEW_ABSTRACTED_ARG]])
  var minimallyAbstracted = transform
  consumeGenericInOut(&minimallyAbstracted)
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0SiSdIxyd_SiSdIxir_TR : $@convention(thin) (@in Int, @owned @callee_owned (Int) -> Double) -> @out Double
// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0SiSdIxir_SiSdIxyd_TR : $@convention(thin) (Int, @owned @callee_owned (@in Int) -> @out Double) -> Double

// CHECK-LABEL: sil hidden @_T017reabstract_lvalue0A13MetatypeInOutyyF : $@convention(thin) () -> ()
func reabstractMetatypeInOut() {
  var thinMetatype = MyMetatypeIsThin.self
  // CHECK: [[FUNC:%.*]] = function_ref @_T017reabstract_lvalue19consumeGenericInOut{{[_0-9a-zA-Z]*}}F
  // CHECK: [[BOX:%.*]] = alloc_stack $@thick MyMetatypeIsThin.Type
  // CHECK: [[THICK:%.*]] = metatype $@thick MyMetatypeIsThin.Type
  // CHECK: store [[THICK]] to [trivial] [[BOX]]
  // CHECK: apply [[FUNC]]<MyMetatypeIsThin.Type>([[BOX]])
  consumeGenericInOut(&thinMetatype)
}
