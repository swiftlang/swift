// RUN: %target-swift-frontend -emit-silgen -enforce-exclusivity=checked %s | %FileCheck %s

class C {}

struct A {}
struct B { var owner: C }

var a = A()

// CHECK: assign {{%.*}} to {{%.*}} : $*A
// CHECK: destroy_value {{%.*}} : $B
(a, _) = (A(), B(owner: C()))

class D { var child: C = C() }

// Verify that the LHS is formally evaluated before the RHS.
// CHECK-LABEL: sil hidden @_T010assignment5test1yyF : $@convention(thin) () -> () {
func test1() {
  // CHECK: [[CTOR:%.*]] = function_ref @_T010assignment1DC{{[_0-9a-zA-Z]*}}fC
  // CHECK: [[T0:%.*]] = metatype $@thick D.Type
  // CHECK: [[D:%.*]] = apply [[CTOR]]([[T0]])
  // CHECK: [[SETTER:%.*]] = class_method [[D]] : $D,  #D.child!setter.1
  // CHECK: [[CTOR:%.*]] = function_ref @_T010assignment1CC{{[_0-9a-zA-Z]*}}fC
  // CHECK: [[T0:%.*]] = metatype $@thick C.Type
  // CHECK: [[C:%.*]] = apply [[CTOR]]([[T0]])
  // CHECK: apply [[SETTER]]([[C]], [[D]])
  D().child = C()
}

// rdar://32039566
protocol P {
  var left: Int {get set}
  var right: Int {get set}
}

// Verify that the access to the LHS does not begin until after the
// RHS is formally evaluated.
// CHECK-LABEL: sil hidden @_T010assignment15copyRightToLeftyAA1P_pz1p_tF : $@convention(thin) (@inout P) -> () {
func copyRightToLeft(p: inout P) {
  // CHECK: bb0(%0 : $*P):
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] %0 : $*P
  // CHECK:   [[READ_OPEN:%.*]] = open_existential_addr immutable_access [[READ]]
  // CHECK:   end_access [[READ]] : $*P
  // CHECK:   [[WRITE:%.*]] = begin_access [modify] [unknown] %0 : $*P
  // CHECK:   [[WRITE_OPEN:%.*]] = open_existential_addr mutable_access [[WRITE]]
  // CHECK:   end_access [[WRITE]] : $*P
  p.left = p.right
}
