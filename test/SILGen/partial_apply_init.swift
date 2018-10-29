// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden @$s18partial_apply_init06class_c1_a1_B0{{[_0-9a-zA-Z]*}}F
func class_init_partial_apply(c: C.Type) {
  // Partial applications at the static metatype use the direct (_TTd) thunk.
  // CHECK: function_ref @$s18partial_apply_init1CC{{[_0-9a-zA-Z]*}}fCTcTd
  let xC: (Int) -> C = C.init
  // CHECK: function_ref @$s18partial_apply_init1CC{{[_0-9a-zA-Z]*}}fCTcTd
  let requiredC: (Double) -> C = C.init

  // Partial applications to a dynamic metatype must be dynamically dispatched and use
  // the normal thunk.
  // CHECK: function_ref @$s18partial_apply_init1CC{{[_0-9a-zA-Z]*}}fC
  let requiredM: (Double) -> C = c.init
}

// CHECK-LABEL: sil shared [thunk] @$s18partial_apply_init1CC{{[_0-9a-zA-Z]*}}fCTcTd
// CHECK:         function_ref @$s18partial_apply_init1CC{{[_0-9a-zA-Z]*}}fC
// CHECK-LABEL: sil shared [thunk] @$s18partial_apply_init1CC{{[_0-9a-zA-Z]*}}fCTcTd
// CHECK:         function_ref @$s18partial_apply_init1CC{{[_0-9a-zA-Z]*}}fC
// CHECK-LABEL: sil shared [thunk] @$s18partial_apply_init1CC{{[_0-9a-zA-Z]*}}fC
// CHECK:         class_method %0 : $@thick C.Type, #C.init!allocator.1

// CHECK-LABEL: sil hidden @$s18partial_apply_init010archetype_c1_a1_B0{{[_0-9a-zA-Z]*}}F
func archetype_init_partial_apply<T: C>(t: T.Type) where T: P {
  // Archetype initializations are always dynamic, whether applied to the type or a metatype.
  // CHECK: function_ref @$s18partial_apply_init1CC{{[_0-9a-zA-Z]*}}fC
  let requiredT: (Double) -> T = T.init
  // CHECK: function_ref @$s18partial_apply_init1PP{{[_0-9a-zA-Z]*}}fC
  let protoT: (String) -> T = T.init
  // CHECK: function_ref @$s18partial_apply_init1PPAAE{{[_0-9a-zA-Z]*}}fC
  let protoExtT: (Float) -> T = T.init

  // CHECK: function_ref @$s18partial_apply_init1CC{{[_0-9a-zA-Z]*}}fC
  let requiredM: (Double) -> T = t.init
  // CHECK: function_ref @$s18partial_apply_init1PP{{[_0-9a-zA-Z]*}}fC
  let protoM: (String) -> T = t.init
  // CHECK: function_ref @$s18partial_apply_init1PPAAE{{[_0-9a-zA-Z]*}}fC
  let protoExtM: (Float) -> T = t.init
}

// CHECK-LABEL: sil shared [thunk] @$s18partial_apply_init1PP{{[_0-9a-zA-Z]*}}fC
// CHECK:         witness_method $Self, #P.init!allocator.1
// CHECK-LABEL: sil shared [thunk] @$s18partial_apply_init1PPAAE{{[_0-9a-zA-Z]*}}fC
// CHECK:         function_ref @$s18partial_apply_init1PPAAE{{[_0-9a-zA-Z]*}}fC

