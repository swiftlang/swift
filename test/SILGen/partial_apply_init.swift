// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -suppress-argument-labels-in-types %s | %FileCheck %s

class C {
  init(x: Int) {}

  required init(required: Double) {}
}

class D {
  required init(required: Double) {}
}

protocol P {
  init(proto: String)
}

extension P {
  init(protoExt: Float) {
    self.init(proto: "")
  }
}

// CHECK-LABEL: sil hidden @_TF18partial_apply_init24class_init_partial_apply
func class_init_partial_apply(c: C.Type) {
  // Partial applications at the static metatype use the direct (_TTd) thunk.
  // CHECK: function_ref @_TTdFC18partial_apply_init1CC
  let xC: (Int) -> C = C.init
  // CHECK: function_ref @_TTdFC18partial_apply_init1CC
  let requiredC: (Double) -> C = C.init

  // Partial applications to a dynamic metatype must be dynamically dispatched and use
  // the normal thunk.
  // CHECK: function_ref @_TFC18partial_apply_init1CC
  let requiredM: (Double) -> C = c.init
}

// CHECK-LABEL: sil shared [thunk] @_TTdFC18partial_apply_init1CC
// CHECK:         function_ref @_TFC18partial_apply_init1CC
// CHECK-LABEL: sil shared [thunk] @_TTdFC18partial_apply_init1CC
// CHECK:         function_ref @_TFC18partial_apply_init1CC
// CHECK-LABEL: sil shared [thunk] @_TFC18partial_apply_init1CC
// CHECK:         class_method %0 : $@thick C.Type, #C.init!allocator.1

// CHECK-LABEL: sil hidden @_TF18partial_apply_init28archetype_init_partial_apply
func archetype_init_partial_apply<T: C where T: P>(t: T.Type) {
  // Archetype initializations are always dynamic, whether applied to the type or a metatype.
  // CHECK: function_ref @_TFC18partial_apply_init1CC
  let requiredT: (Double) -> T = T.init
  // CHECK: function_ref @_TFP18partial_apply_init1PC
  let protoT: (String) -> T = T.init
  // CHECK: function_ref @_TFE18partial_apply_initPS_1PC
  let protoExtT: (Float) -> T = T.init

  // CHECK: function_ref @_TFC18partial_apply_init1CC
  let requiredM: (Double) -> T = t.init
  // CHECK: function_ref @_TFP18partial_apply_init1PC
  let protoM: (String) -> T = t.init
  // CHECK: function_ref @_TFE18partial_apply_initPS_1PC
  let protoExtM: (Float) -> T = t.init
}

// CHECK-LABEL: sil shared [thunk] @_TFP18partial_apply_init1PC
// CHECK:         witness_method $Self, #P.init!allocator.1
// CHECK-LABEL: sil shared [thunk] @_TFE18partial_apply_initPS_1PC
// CHECK:         function_ref @_TFE18partial_apply_initPS_1PC

