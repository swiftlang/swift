// RUN: %swift %s -emit-silgen | FileCheck %s
struct X { }

class A {
  // CHECK-LABEL: sil @_TFC20complete_object_init1ACfMS0_FT_S0_ : $@thin (@thick A.Type) -> @owned A
  init() -> Self {
    // CHECK: bb0([[SELF_META:%[0-9]+]] : $@thick A.Type):
    // CHECK:   [[SELF:%[0-9]+]] = alloc_ref_dynamic [[SELF_META]] : $@thick A.Type, $A
    // CHECK:   [[OTHER_INIT:%[0-9]+]] = function_ref @_TFC20complete_object_init1AcfMS0_FT_S0_ : $@cc(method) @thin (@owned A) -> @owned A
    // CHECK:   [[RESULT:%[0-9]+]] = apply [[OTHER_INIT]]([[SELF]]) : $@cc(method) @thin (@owned A) -> @owned A
    // CHECK:   return [[RESULT]] : $A
    self.init withX(X())
  }

  init withX(X) { }
}

