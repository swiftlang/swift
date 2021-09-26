
// RUN: %target-swift-emit-silgen -enable-experimental-lexical-lifetimes -module-name borrow -parse-stdlib %s | %FileCheck %s

import Swift

final class D {}

// Make sure that we insert the borrow for a ref_element_addr lvalue in the
// proper place.
final class C {
  init() {}
  init?(failably: ()) {}
  var d: D = D()
}

func use<T>(_ t: T) {}
func useD(_ d: D) {}

// CHECK-LABEL: sil hidden [ossa] @$s6borrow44lvalueBorrowShouldBeAtEndOfFormalAccessScope{{.*}} : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:   [[BOX:%.*]] = alloc_box ${ var C }, var, name "c"
// CHECK:   [[PB_BOX:%.*]] = project_box [[BOX]]
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
// CHECK:   destroy_value [[BOX]]
// CHECK: } // end sil function '$s6borrow44lvalueBorrowShouldBeAtEndOfFormalAccessScope{{.*}}'
func lvalueBorrowShouldBeAtEndOfFormalAccessScope() {
  var c = C()
  useD(c.d)
}

// CHECK-LABEL: sil hidden [ossa] @lexical_borrow_let_class
// CHECK:   [[INIT_C:%[^,]+]] = function_ref @$s6borrow1CCACycfC
// CHECK:   [[INSTANCE:%[^,]+]] = apply [[INIT_C]]({{%[0-9]+}})
// CHECK:   [[BORROW:%[^,]+]] = begin_borrow [lexical] [[INSTANCE]] : $C
// CHECK:   end_borrow [[BORROW:%[^,]+]]
// CHECK-LABEL: } // end sil function 'lexical_borrow_let_class'
@_silgen_name("lexical_borrow_let_class")
func lexical_borrow_let_class() {
  let c = C()
}

// CHECK-LABEL: sil hidden [ossa] @lexical_borrow_if_let_class
// CHECK:   [[INIT_C:%[^,]+]] = function_ref @$s6borrow1CC8failablyACSgyt_tcfC
// CHECK:   [[INSTANCE:%[^,]+]] = apply [[INIT_C]]({{%[^,]+}})
// CHECK:   switch_enum [[INSTANCE]] : $Optional<C>, case #Optional.some!enumelt: [[BASIC_BLOCK2:bb[^,]+]], case #Optional.none!enumelt: {{bb[^,]+}}
// CHECK: [[BASIC_BLOCK2]]([[INSTANCE:%[^,]+]] : @owned $C):
// CHECK:   [[BORROW:%[^,]+]] = begin_borrow [lexical] [[INSTANCE]] : $C
// CHECK:   end_borrow [[BORROW]] : $C
// CHECK-LABEL: // end sil function 'lexical_borrow_if_let_class'
@_silgen_name("lexical_borrow_if_let_class")
func lexical_borrow_if_let_class() {
  if let c = C(failably: ()) {
    use(())
  }
}

struct S {
  let c: C
}

// CHECK-LABEL: sil hidden [ossa] @lexical_borrow_let_class_in_struct
// CHECK:   [[INIT_S:%[^,]+]] = function_ref @$s6borrow1SV1cAcA1CC_tcfC
// CHECK:   [[INSTANCE:%[^,]+]] = apply [[INIT_S]]({{%[0-9]+}}, {{%[0-9]+}})
// CHECK:   [[BORROW:%[^,]+]] = begin_borrow [lexical] [[INSTANCE]] : $S
// CHECK:   end_borrow [[BORROW:%[^,]+]]
// CHECK-LABEL: } // end sil function 'lexical_borrow_let_class_in_struct'
@_silgen_name("lexical_borrow_let_class_in_struct")
func lexical_borrow_let_class_in_struct() {
  let s = S(c: C())
}

enum E {
  case e(C)
}

// CHECK-LABEL: sil hidden [ossa] @lexical_borrow_let_class_in_enum
// CHECK:   [[INSTANCE:%[^,]+]] = enum $E, #E.e!enumelt, {{%[0-9]+}} : $C
// CHECK:   [[BORROW:%[^,]+]] = begin_borrow [lexical] [[INSTANCE]] : $E
// CHECK:   end_borrow [[BORROW:%[^,]+]]
// CHECK-LABEL: } // end sil function 'lexical_borrow_let_class_in_enum'
@_silgen_name("lexical_borrow_let_class_in_enum")
func lexical_borrow_let_class_in_enum() {
  let s = E.e(C())
}
