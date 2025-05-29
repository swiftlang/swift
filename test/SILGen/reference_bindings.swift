// RUN: %target-swift-frontend -emit-silgen -enable-experimental-feature ReferenceBindings -o - %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -sil-verify-all -enable-experimental-feature ReferenceBindings -o - %s | %FileCheck -check-prefix=SIL %s

// REQUIRES: swift_feature_ReferenceBindings

class Klass {}
struct S {
  var k = Klass()
}

func doSomething() {}

// Before the transformation, our access scope is not to end of scope... so
// doSomething() is not within the access scope.
//
// CHECK-LABEL: sil hidden [ossa] @$s18reference_bindings13testBindToVaryyF : $@convention(thin) () -> () {
// CHECK: [[BOX:%.*]] = alloc_box ${ var Int }, var, name "x"
// CHECK: [[LIFETIME:%.*]] = begin_borrow [var_decl] %0
// CHECK: [[PROJECT:%.*]] = project_box [[LIFETIME]]
// CHECK: [[INOUT_BOX:%.*]] = alloc_box ${ var Int }, var, name "x2"
// CHECK: [[UNRESOLVED_BINDING:%.*]] = mark_unresolved_reference_binding [inout] [[INOUT_BOX]]
// CHECK: [[INOUT_LIFETIME:%.*]] = begin_borrow [var_decl] [[UNRESOLVED_BINDING]]
// CHECK: [[INOUT_PROJECT:%.*]] = project_box [[INOUT_LIFETIME]]
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT]]
// CHECK: copy_addr [[ACCESS]] to [init] [[INOUT_PROJECT]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[FUNC:%.*]] = function_ref @$s18reference_bindings11doSomethingyyF : $@convention(thin) () -> ()
// CHECK: apply [[FUNC]]()
// CHECK: destroy_value [[UNRESOLVED_BINDING]]
// CHECK: destroy_value [[BOX]]
// CHECK: } // end sil function '$s18reference_bindings13testBindToVaryyF'

// SIL: sil hidden @$s18reference_bindings13testBindToVaryyF : $@convention(thin) () -> () {
// SIL: bb0:
// SIL:   [[BOX:%.*]] = alloc_stack [var_decl] $Int, var, name "x"
// SIL:   [[INOUT_BOX:%.*]] = alloc_stack [var_decl] $Int, var, name "x2"
// SIL:   [[ACCESS:%.*]] = begin_access [modify] [static] [[BOX]]
// SIL:   store {{%.*}} to [[INOUT_BOX]]
// SIL:   [[FUNC:%.*]] = function_ref @$s18reference_bindings11doSomethingyyF : $@convention(thin) () -> ()
// SIL:   apply [[FUNC]]()
// SIL:   store {{%.*}} to [[ACCESS]]
// SIL:   end_access [[ACCESS]]
// SIL: } // end sil function '$s18reference_bindings13testBindToVaryyF'
func testBindToVar() {
    var x = 5
    inout x2 = x
    doSomething()
}

// CHECK-LABEL: sil hidden [ossa] @$s18reference_bindings15testBindToInOutyySSzF : $@convention(thin) (@inout String) -> () {
// CHECK: bb0([[ARG:%.*]] : $*String):
// CHECK:   [[BOX:%.*]] = alloc_box ${ var String }
// CHECK:   [[MARK:%.*]] = mark_unresolved_reference_binding [inout] [[BOX]]
// CHECK:   [[LIFETIME:%.*]] = begin_borrow [var_decl] [[MARK]]
// CHECK:   [[PROJECT:%.*]] = project_box [[LIFETIME]]
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[ARG]]
// CHECK:   copy_addr [[ACCESS]] to [init] [[PROJECT]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   apply {{%.*}}()
// CHECK:   destroy_value [[MARK]]
// CHECK: } // end sil function '$s18reference_bindings15testBindToInOutyySSzF'

// SIL-LABEL: sil hidden @$s18reference_bindings15testBindToInOutyySSzF : $@convention(thin) (@inout String) -> () {
// SIL: bb0([[ARG:%.*]] : $*String):
// SIL:   [[STACK:%.*]] = alloc_stack [var_decl] $String
// SIL:   [[ACCESS:%.*]] = begin_access [modify] [static] [[ARG]]
// SIL:   copy_addr [take] [[ACCESS]] to [init] [[STACK]]
// SIL:   apply {{%.*}}
// SIL:   copy_addr [take] [[STACK]] to [init] [[ACCESS]]
// SIL:   end_access [[ACCESS]]
// SIL:   dealloc_stack [[STACK]]
// SIL: } // end sil function '$s18reference_bindings15testBindToInOutyySSzF'
func testBindToInOut(_ x: inout String) {
  inout y = x
  doSomething()
}
