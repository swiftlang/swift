// RUN: %swift -emit-silgen %s | FileCheck %s

class C {
  @abstract init() { }
}

// CHECK-LABEL: sil @_TF12dynamic_init15testDynamicInitFT2cmMCS_1C_T_ : $@thin (@thick C.metatype) -> () 
func testDynamicInit(cm: C.metatype) {
  // CHECK: bb0([[CM:%[0-9]+]] : $@thick C.metatype):
  // CHECK:   [[METHOD:%[0-9]+]] = class_method [[CM]] : $@thick C.metatype, #C.init!allocator.1 : $@thin (@thick C.metatype) -> @owned C
  // CHECK:   [[C_OBJ:%[0-9]+]] = apply [[METHOD]]([[CM]]) : $@thin (@thick C.metatype) -> @owned C
  // CHECK:   strong_release [[C_OBJ]] : $C
  // CHECK:   [[RESULT:%[0-9]+]] = tuple ()
  // CHECK:   return [[RESULT]] : $()
  cm()
}

// CHECK-LABEL: sil @_TF12dynamic_init14testStaticInitFT_T_ : $@thin () -> ()
func testStaticInit() {
  // CHECK-NOT: class_method
  // CHECK: function_ref @_TFC12dynamic_init1CCfMS0_FT_S0_ : $@thin (@thick C.metatype) -> @owned C
  C()
  // CHECK-NOT: class_method
  // CHECK: return
}
