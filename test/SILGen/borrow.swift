
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name borrow -parse-stdlib %s | %FileCheck %s

import Swift

final class D {}

// Make sure that we insert the borrow for a ref_element_addr lvalue in the
// proper place.
final class C {
  init() {}
  init?(failably: ()) {}
  var d: D = D()
}

func useD(_ d: D) {}

// CHECK-LABEL: sil hidden [ossa] @$s6borrow44lvalueBorrowShouldBeAtEndOfFormalAccessScope{{.*}} : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[BOX:%.*]] = alloc_box ${ var C }, var, name "c"
// CHECK:   [[BOX_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[BOX]]
// CHECK:   [[PB_BOX:%.*]] = project_box [[BOX_LIFETIME]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PB_BOX]] : $*C
// CHECK:   [[CLASS:%.*]] = load [copy] [[ACCESS]]
// CHECK:   [[BORROWED_CLASS:%.*]] = begin_borrow [[CLASS]]
// CHECK:   [[OFFSET:%.*]] = ref_element_addr [[BORROWED_CLASS]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [dynamic] [[OFFSET]] : $*D
// CHECK:   [[LOADED_VALUE:%.*]] = load [copy] [[ACCESS]]
// CHECK:   end_borrow [[BORROWED_CLASS]]
// CHECK:   destroy_value [[CLASS]]
// CHECK:   [[FUNC:%.*]] = function_ref @$s6borrow4useD{{.*}} : $@convention(thin) (@guaranteed D) -> ()
// CHECK:   apply [[FUNC]]([[LOADED_VALUE]])
// CHECK:   end_borrow [[BOX_LIFETIME]]
// CHECK:   destroy_value [[BOX]]
// CHECK: } // end sil function '$s6borrow44lvalueBorrowShouldBeAtEndOfFormalAccessScope{{.*}}'
func lvalueBorrowShouldBeAtEndOfFormalAccessScope() {
  var c = C()
  useD(c.d)
}
