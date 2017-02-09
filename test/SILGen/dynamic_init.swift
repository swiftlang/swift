// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

class C {
  required init() { }
}

// CHECK-LABEL: sil hidden @_T012dynamic_init15testDynamicInit{{[_0-9a-zA-Z]*}}F
func testDynamicInit(cm: C.Type) {
  // CHECK: bb0([[CM:%[0-9]+]] : $@thick C.Type):
  // CHECK:   [[METHOD:%[0-9]+]] = class_method [[CM]] : $@thick C.Type, #C.init!allocator.1 : (C.Type) -> () -> C, $@convention(method) (@thick C.Type) -> @owned C
  // CHECK:   [[C_OBJ:%[0-9]+]] = apply [[METHOD]]([[CM]]) : $@convention(method) (@thick C.Type) -> @owned C
  // CHECK:   destroy_value [[C_OBJ]] : $C
  // CHECK:   [[RESULT:%[0-9]+]] = tuple ()
  // CHECK:   return [[RESULT]] : $()
  cm.init()
}

// CHECK-LABEL: sil hidden @_T012dynamic_init14testStaticInit{{[_0-9a-zA-Z]*}}F
func testStaticInit() {
  // CHECK-NOT: class_method
  // CHECK: function_ref @_T012dynamic_init1CC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick C.Type) -> @owned C
  C()
  // CHECK-NOT: class_method
  // CHECK: return
}
