// RUN: %target-swift-frontend %s -emit-silgen | FileCheck %s

struct X { }

class A {
  // CHECK-LABEL: sil hidden @_TFC20complete_object_init1AcfMS0_FT_S0_ : $@convention(method) (@owned A) -> @owned A
// CHECK: bb0([[SELF_PARAM:%[0-9]+]] : $A):
// CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box $A
// CHECK:   [[SELF:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1 : $*A
// CHECK:   store [[SELF_PARAM]] to [[SELF]] : $*A
// CHECK:   [[SELFP:%[0-9]+]] = load [[SELF]] : $*A
// CHECK:   [[INIT:%[0-9]+]] = class_method [[SELFP]] : $A, #A.init!initializer.1 : A.Type -> (x: X) -> A , $@convention(method) (X, @owned A) -> @owned A
// CHECK:   [[X_INIT:%[0-9]+]] = function_ref @_TFV20complete_object_init1XCfMS0_FT_S0_ : $@convention(thin) (@thin X.Type) -> X
// CHECK:   [[X_META:%[0-9]+]] = metatype $@thin X.Type
// CHECK:   [[X:%[0-9]+]] = apply [[X_INIT]]([[X_META]]) : $@convention(thin) (@thin X.Type) -> X
// CHECK:   [[INIT_RESULT:%[0-9]+]] = apply [[INIT]]([[X]], [[SELFP]]) : $@convention(method) (X, @owned A) -> @owned A
// CHECK:   store [[INIT_RESULT]] to [[SELF]] : $*A
// CHECK:   [[RESULT:%[0-9]+]] = load [[SELF]] : $*A
// CHECK:   strong_retain [[RESULT]] : $A
// CHECK:   strong_release [[SELF_BOX]]#0 : $@box A
// CHECK:   return [[RESULT]] : $A

  // CHECK-LABEL: sil hidden @_TFC20complete_object_init1ACfMS0_FT_S0_ : $@convention(thin) (@thick A.Type) -> @owned A
  convenience init() {
    // CHECK: bb0([[SELF_META:%[0-9]+]] : $@thick A.Type):
    // CHECK:   [[SELF:%[0-9]+]] = alloc_ref_dynamic [[SELF_META]] : $@thick A.Type, $A
    // CHECK:   [[OTHER_INIT:%[0-9]+]] = function_ref @_TFC20complete_object_init1AcfMS0_FT_S0_ : $@convention(method) (@owned A) -> @owned A
    // CHECK:   [[RESULT:%[0-9]+]] = apply [[OTHER_INIT]]([[SELF]]) : $@convention(method) (@owned A) -> @owned A
    // CHECK:   return [[RESULT]] : $A
    self.init(x: X())
  }

  init(x: X) { }
}

