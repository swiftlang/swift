// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

class C {
  required init() { }
}

// CHECK-LABEL: sil hidden @_TF12dynamic_init15testDynamicInit
func testDynamicInit(cm: C.Type) {
  // CHECK: bb0([[CM:%[0-9]+]] : $@thick C.Type):
  // CHECK:   [[METHOD:%[0-9]+]] = class_method [[CM]] : $@thick C.Type, #C.init!allocator.1 : C.Type -> () -> C , $@thin (@thick C.Type) -> @owned C
  // CHECK:   [[C_OBJ:%[0-9]+]] = apply [[METHOD]]([[CM]]) : $@thin (@thick C.Type) -> @owned C
  // CHECK:   strong_release [[C_OBJ]] : $C
  // CHECK:   [[RESULT:%[0-9]+]] = tuple ()
  // CHECK:   return [[RESULT]] : $()
  cm()
}

// CHECK-LABEL: sil hidden @_TF12dynamic_init14testStaticInit
func testStaticInit() {
  // CHECK-NOT: class_method
  // CHECK: function_ref @_TFC12dynamic_init1CCfMS0_FT_S0_ : $@thin (@thick C.Type) -> @owned C
  C()
  // CHECK-NOT: class_method
  // CHECK: return
}
