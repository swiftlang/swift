// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

struct X { }

class A {
// CHECK-LABEL: sil hidden [ossa] @$s20complete_object_init1AC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick A.Type) -> @owned A
// CHECK: bb0([[SELF_META:%[0-9]+]] : $@thick A.Type):
// CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box ${ var A }
// CHECK:   [[UNINIT_SELF:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]] : ${ var A }
// CHECK:   [[UNINIT_SELF_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[UNINIT_SELF]]
// CHECK:   [[PB:%.*]] = project_box [[UNINIT_SELF_LIFETIME]]
// CHECK:   [[INIT:%[0-9]+]] = class_method [[SELF_META]] : $@thick A.Type, #A.init!allocator
// CHECK:   [[INIT_RESULT:%[0-9]+]] = apply [[INIT]]({{%[^,]*}}, [[SELF_META]])
// CHECK:   assign [[INIT_RESULT]] to [[PB]] : $*A
// CHECK:   [[RESULT:%[0-9]+]] = load [copy] [[PB]] : $*A
// CHECK:   end_borrow [[UNINIT_SELF_LIFETIME]]
// CHECK:   destroy_value [[UNINIT_SELF]] : ${ var A }
// CHECK:   return [[RESULT]] : $A
  convenience init() {
    self.init(x: X())
  }

  init(x: X) { }
}

