// RUN: %target-swift-emit-silgen %s -enable-sil-ownership | %FileCheck %s

struct X { }

class A {
// CHECK-LABEL: sil hidden @$S20complete_object_init1AC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick A.Type) -> @owned A
// CHECK: bb0([[SELF_META:%[0-9]+]] : @trivial $@thick A.Type):
// CHECK:   [[SELF:%[0-9]+]] = alloc_ref_dynamic [[SELF_META]] : $@thick A.Type, $A
// CHECK:   [[OTHER_INIT:%[0-9]+]] = function_ref @$S20complete_object_init1AC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (@owned A) -> @owned A
// CHECK:   [[RESULT:%[0-9]+]] = apply [[OTHER_INIT]]([[SELF]]) : $@convention(method) (@owned A) -> @owned A
// CHECK:   return [[RESULT]] : $A

// CHECK-LABEL: sil hidden @$S20complete_object_init1AC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (@owned A) -> @owned A
// CHECK: bb0([[SELF_PARAM:%[0-9]+]] : @owned $A):
// CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box ${ var A }
// CHECK:   [[UNINIT_SELF:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]] : ${ var A }
// CHECK:   [[PB:%.*]] = project_box [[UNINIT_SELF]]
// CHECK:   store [[SELF_PARAM]] to [init] [[PB]] : $*A
// CHECK:   [[SELFP:%[0-9]+]] = load [take] [[PB]] : $*A
// CHECK:   [[X_META:%[0-9]+]] = metatype $@thin X.Type
// CHECK:   [[X_INIT:%[0-9]+]] = function_ref @$S20complete_object_init1XV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thin X.Type) -> X
// CHECK:   [[X:%[0-9]+]] = apply [[X_INIT]]([[X_META]]) : $@convention(method) (@thin X.Type) -> X
// CHECK:   [[INIT:%[0-9]+]] = class_method [[SELFP]] : $A, #A.init!initializer.1 : (A.Type) -> (X) -> A, $@convention(method) (X, @owned A) -> @owned A
// CHECK:   [[INIT_RESULT:%[0-9]+]] = apply [[INIT]]([[X]], [[SELFP]]) : $@convention(method) (X, @owned A) -> @owned A
// CHECK:   store [[INIT_RESULT]] to [init] [[PB]] : $*A
// CHECK:   [[RESULT:%[0-9]+]] = load [copy] [[PB]] : $*A
// CHECK:   destroy_value [[UNINIT_SELF]] : ${ var A }
// CHECK:   return [[RESULT]] : $A
  convenience init() {
    self.init(x: X())
  }

  init(x: X) { }
}

